use rtl::*;
use rtl::label::*;
use common::register::*;
use rtl::ops::*;
use std::iter::FromIterator;
use typing::ast as tast;
use std::collections::{HashMap, HashSet};
//use std::boxed::Box;
//use std::ops::Deref;

impl File {
    pub fn from_typer_ast(typer_ast: tast::File) -> Result<File, String> {
        let mut file = File{globals: Vec::new(), functions: Vec::new()};
        file.globals = Vec::from_iter(typer_ast.variables.keys().cloned());
        for function in typer_ast.function_definitions {
            match FuncDefinition::from_typer_function(function, &typer_ast.types/*, &file.globals*/) {
                Ok(func_def) => {file.functions.push(func_def);},
                Err(e) => {return Err(e);},

            }
        }
        Ok(file)

    }
}

struct FuncDefinitionBuilder<'a> {
    label_allocator: LabelAllocator,
    register_allocator: RegisterAllocator,
    result: Register,
    exit: Label,
    variables: Vec<HashMap<String, Register>>,
    locals: HashSet<Register>,
    instructions: HashMap<Label, Instruction>,
    formals: Vec<Register>,
    types: &'a HashMap<String, tast::Struct>,
}

impl<'a, 'b> FuncDefinitionBuilder<'a> {
    fn new(f: &'b tast::Function,  types: &'a HashMap<String, tast::Struct>) -> Self {
        let mut label_allocator = LabelAllocator::new();
        let mut register_allocator = RegisterAllocator::new();
        let result = register_allocator.fresh();
        let exit = label_allocator.fresh();
        let mut var_registers = HashMap::<String, Register>::new();
        let mut locals = HashSet::new();
        let mut formals = Vec::new();
        for param in f.params.clone() {
            var_registers.insert(param.clone(), register_allocator.fresh());
            locals.insert(var_registers[&param].clone());
            formals.push(var_registers[&param].clone());
        }

        FuncDefinitionBuilder{
            label_allocator: label_allocator,
            register_allocator: register_allocator,
            result: result,
            exit: exit,
            variables: vec![var_registers],
            locals: locals,
            instructions: HashMap::new(),
            formals: formals,
            types: types,
        }

    }

    fn bloc(& mut self, bloc: tast::Bloc, exit: Label) -> Result<Label, String> {
        let mut var_registers = HashMap::new();
        for var in bloc.decls {
            var_registers.insert(var.0.clone(), self.register_allocator.fresh());
            self.locals.insert(var_registers[&var.0]);
        }
        let mut current_exit = exit;
        self.variables.push(var_registers);
        let mut stmts = bloc.stmts;
        stmts.reverse();
        for stmt in stmts {
            match self.statement(stmt, current_exit) {
                Ok(new_exit) => {
                    current_exit = new_exit;
                },
                Err(e) => {return Err(e);},
            }
        }
        let res = Ok(current_exit);
        self.variables.pop();
        res
    }

    fn statement(& mut self, stmt: tast::Statement, exit: Label) -> Result<Label, String> {
        match stmt {
            tast::Statement::Return(expr) => {
                let result = self.result;
                let exit = self.exit;
                self.expression(expr, exit, Some(result))
            },
            tast::Statement::Expr(expr) => {
                self.expression(expr, exit, None)
            },
            tast::Statement::Bloc(blk) => {
                self.bloc(blk, exit)
            },
            tast::Statement::Noop => {Ok(exit)},
            tast::Statement::If(condition, box_stmt) => {
                let label = try!(self.statement(*box_stmt, exit.clone()));
                // Naïve implementation !
                let label2 = self.label_allocator.fresh();
                let cnd_reg = self.register_allocator.fresh();
                let label3 = self.label_allocator.fresh();
                self.instructions.insert(label2.clone(), Instruction::Branch(x64Branch::je, exit, label));
                self.instructions.insert(label3.clone(), Instruction::BinaryOp(x64BinaryOp::test, cnd_reg, cnd_reg, label2));
                self.expression(condition, label3, Some(cnd_reg))
            },
            tast::Statement::IfElse(condition, box_stmt_if, box_stmt_else) => {
                let label_else = try!(self.statement(*box_stmt_else, exit.clone()));
                let label_if = try!(self.statement(*box_stmt_if, exit.clone()));
                // Naïve implementation !
                let label2 = self.label_allocator.fresh();
                let cnd_reg = self.register_allocator.fresh();
                let label3 = self.label_allocator.fresh();
                self.instructions.insert(label2.clone(), Instruction::Branch(x64Branch::je, label_else, label_if));
                self.instructions.insert(label3.clone(), Instruction::BinaryOp(x64BinaryOp::test, cnd_reg, cnd_reg, label2));
                self.expression(condition, label3, Some(cnd_reg))
            },
            tast::Statement::While(condition, box_stmt) => {
                let label3 = self.label_allocator.fresh();
                let label2 = self.label_allocator.fresh();
                let cnd_reg = self.register_allocator.fresh();
                let loop_begin = try!(self.expression(condition, label3.clone(), Some(cnd_reg)));
                let label_loop_body = try!(self.statement(*box_stmt, loop_begin.clone()));
                self.instructions.insert(label2.clone(), Instruction::Branch(x64Branch::je, exit, label_loop_body));
                self.instructions.insert(label3, Instruction::BinaryOp(x64BinaryOp::test, cnd_reg, cnd_reg, label2));
                Ok(loop_begin)
            },
            //_ => Err(String::from("Unimplemented")),
        }
    }

    fn expr_const(& mut self, ivalue: i64, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label = self.label_allocator.fresh();
            self.instructions.insert(label.clone(), Instruction::Const(ivalue, result, exit));
            Ok(label)
        } else {
            Ok(exit) // Noop !
        }
    }

    fn expr_lvalue(& mut self, name: String, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label = self.label_allocator.fresh();
            if let Some(src_register) = self.find_var(&name) {
                self.instructions.insert(label.clone(), Instruction::BinaryOp(x64BinaryOp::mov, src_register, result, exit));
            } else { // Global Variable
                self.instructions.insert(label.clone(), Instruction::AccessGlobal(name, result, exit));
            }
            Ok(label)
        } else {
            Ok(exit) // Noop !
        }
    }

    fn expr_sizeof(& mut self, typename: String, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label = self.label_allocator.fresh();
            let ivalue = self.types[&typename].size();
            self.instructions.insert(label.clone(), Instruction::Const(ivalue, result, exit));
            Ok(label)
        } else {
            Ok(exit) // Noop !
        }
    }

    fn expr_memb_deref(& mut self, box_expr: Box<tast::Expression>, membername: String, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let tast::Type::Struct(typename) = box_expr.typ.clone() {
            let typ = &self.types[&typename];
            let index = typ.index[&membername];
            if let Some(result) = result_reg {
                let label = self.label_allocator.fresh();
                let src_register = self.register_allocator.fresh();
                self.instructions.insert(label.clone(), Instruction::Load(src_register, index as i64 * 8 , result, exit));
                self.expression(*box_expr, label, Some(src_register))
            } else {
                self.expression(*box_expr, exit, None)
            }
        } else {
            panic!("Dereferencing of non struct type, typer failed.");
        }
    }

    fn expr_unary(& mut self, op: tast::UnaryOp, box_expr: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        match op {
            tast::UnaryOp::Not => {
                if let Some(result) = result_reg {
                    let label1 = self.label_allocator.fresh();
                    let label2 = self.label_allocator.fresh();
                    let src_register = self.register_allocator.fresh();
                    self.instructions.insert(label1.clone(), Instruction::BinaryOp(x64BinaryOp::test, src_register, src_register, label2.clone()));
                    self.instructions.insert(label2.clone(), Instruction::UnaryOp(x64UnaryOp::sete, result, exit));
                    self.expression(*box_expr, label1, Some(src_register))
                } else {
                    self.expression(*box_expr, exit, None)
                }
            },
            tast::UnaryOp::Minus => {
                if let Some(result) = result_reg {
                    let label1 = self.label_allocator.fresh();
                    let label2 = self.label_allocator.fresh();
                    let src_register = self.register_allocator.fresh();
                    self.instructions.insert(label1.clone(), Instruction::Const(0, result, label2.clone()));
                    self.instructions.insert(label2.clone(), Instruction::BinaryOp(x64BinaryOp::sub, src_register, result, exit));
                    self.expression(*box_expr, label1, Some(src_register))
                } else {
                    self.expression(*box_expr, exit, None)
                }
            }
        }
    }

    // This function is used for lvalue used as destination of affectation.
    fn expr_affect_dest(& mut self, expr: tast::Expression, exit: Label, source_reg: Register) -> Result<Label, String> {
        if expr.kind.lvalue() {
            match expr.kind {
                tast::ExprKind::Lvalue(name) => {
                    let label = self.label_allocator.fresh();
                    if let Some(dest_register) = self.find_var(&name) {
                        self.instructions.insert(label.clone(), Instruction::BinaryOp(x64BinaryOp::mov, source_reg, dest_register, exit));
                    } else { // Global Variable
                        self.instructions.insert(label.clone(), Instruction::AssignGlobal(source_reg, name, exit));
                    }
                    Ok(label)
                },
                tast::ExprKind::MembDeref(box_expr, membername) => {
                    if let tast::Type::Struct(typename) = box_expr.typ.clone() {
                        let typ = &self.types[&typename];
                        let index = typ.index[&membername];
                        let label = self.label_allocator.fresh();
                        let adress = self.register_allocator.fresh();
                        self.instructions.insert(label.clone(), Instruction::Store(source_reg, adress, index as i64 * 8 , exit));
                        self.expression(*box_expr, label, Some(adress))
                    } else {
                        panic!("Dereferencing of non struct type, typer failed.");
                    }
                },
                _ => {Err(String::from("Unkown lvalue type for affectation"))}, // Unreachable in theory.
            }
        } else {
            Err(String::from("Affectation to non lvalue : typer failed"))
        }
    }

    fn expr_bin_affect(& mut self, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        let source_reg = if let Some(result) = result_reg {
            result
        } else {
            self.register_allocator.fresh()
        };

        let res = self.expr_affect_dest(*b_lhs, exit, source_reg);
        match res {
            Err(e) => {Err(e)},
            Ok(label) => self.expression(*b_rhs, label, Some(source_reg))
        }
    }

    fn expr_bin_plus_const(& mut self, ivalue: i64, box_expr: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label1 = self.label_allocator.fresh();
            self.instructions.insert(label1.clone(), Instruction::UnaryOp(x64UnaryOp::addi(ivalue), result, exit));
            self.expression(*box_expr, label1, Some(result))
        } else {
            self.expression(*box_expr, exit, None)
        }

    }

    fn expr_bin_plus(& mut self, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        match (&b_lhs.kind, &b_rhs.kind) {
            (&tast::ExprKind::Const(ivalue), _) => {self.expr_bin_plus_const(ivalue, b_rhs, exit, result_reg)},
            (_, &tast::ExprKind::Const(ivalue)) => {self.expr_bin_plus_const(ivalue, b_lhs, exit, result_reg)},
            _ => {
                if let Some(result) = result_reg {
                    let label2 = self.label_allocator.fresh();
                    let src_register =  self.register_allocator.fresh();
                    let label1 = try!(self.expression(*b_rhs, label2.clone(), Some(src_register)));
                    self.instructions.insert(label2.clone(), Instruction::BinaryOp(x64BinaryOp::add, src_register, result, exit));
                    self.expression(*b_lhs, label1, Some(result))
                } else {
                    let label = try!(self.expression(*b_lhs, exit, None));
                    self.expression(*b_rhs, label, None)
                }
            },
        }
    }

    fn expr_bin_mult(& mut self, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label2 = self.label_allocator.fresh();
            let src_register =  self.register_allocator.fresh();
            let label1 = try!(self.expression(*b_rhs, label2.clone(), Some(src_register)));
            self.instructions.insert(label2.clone(), Instruction::BinaryOp(x64BinaryOp::mul, src_register, result, exit));
            self.expression(*b_lhs, label1, Some(result))
        } else {
            let label = try!(self.expression(*b_lhs, exit, None));
            self.expression(*b_rhs, label, None)
        }
    }

    fn expr_bin_minus(& mut self, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label2 = self.label_allocator.fresh();
            let src_register =  self.register_allocator.fresh();
            let label1 = try!(self.expression(*b_rhs, label2.clone(), Some(src_register)));
            self.instructions.insert(label2.clone(), Instruction::BinaryOp(x64BinaryOp::sub, src_register, result, exit));
            self.expression(*b_lhs, label1, Some(result))
        } else {
            let label = try!(self.expression(*b_lhs, exit, None));
            self.expression(*b_rhs, label, None)
        }
    }

    fn expr_bin_div(& mut self, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label2 = self.label_allocator.fresh();
            let src_register =  self.register_allocator.fresh();
            let label1 = try!(self.expression(*b_rhs, label2.clone(), Some(src_register)));
            self.instructions.insert(label2.clone(), Instruction::BinaryOp(x64BinaryOp::div, src_register, result, exit));
            self.expression(*b_lhs, label1, Some(result))
        } else {
            let label = try!(self.expression(*b_lhs, exit, None));
            self.expression(*b_rhs, label, None)
        }
    }


    fn expr_bin_cmp(& mut self, op: x64UnaryOp, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        if let Some(result) = result_reg {
            let label = self.label_allocator.fresh();
            self.instructions.insert(label.clone(), Instruction::UnaryOp(op, result, exit));
            let label2 = self.label_allocator.fresh();
            let l_reg = self.register_allocator.fresh();
            let r_reg = self.register_allocator.fresh();
            self.instructions.insert(label2.clone(), Instruction::BinaryOp(x64BinaryOp::cmp, r_reg, l_reg, label));
            let label3 = try!(self.expression(*b_rhs, label2, Some(r_reg)));
            self.expression(*b_lhs, label3, Some(l_reg))
        } else {
            let label = try!(self.expression(*b_rhs, exit, None));
            self.expression(*b_lhs, label, None)
        }
    }

    fn cnd_cmp(& mut self, op: x64Branch, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit_false: Label, exit_true: Label) -> Result<Label, String> {
        let label = self.label_allocator.fresh();
        self.instructions.insert(label.clone(), Instruction::Branch(op, exit_true, exit_false));
        let label2 = self.label_allocator.fresh();
        let l_reg = self.register_allocator.fresh();
        let r_reg = self.register_allocator.fresh();
        self.instructions.insert(label2.clone(), Instruction::BinaryOp(x64BinaryOp::cmp, r_reg, l_reg, label));
        let label3 = try!(self.expression(*b_rhs, label2, Some(r_reg)));
        self.expression(*b_lhs, label3, Some(l_reg))
    }

    fn expr_bin_and(& mut self, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        let (result, label) = if let Some(result) = result_reg {
            let cnd_reg = self.register_allocator.fresh();
            let label1 = self.label_allocator.fresh();
            let label2 = self.label_allocator.fresh();
            let label3 = self.label_allocator.fresh();
            self.instructions.insert(label1, Instruction::Const(1, result, exit));
            self.instructions.insert(label2, Instruction::Branch(x64Branch::je, exit, label1));
            self.instructions.insert(label3, Instruction::BinaryOp(x64BinaryOp::test, cnd_reg, cnd_reg, label2));
            (Some(result), try!(self.expression(*b_rhs, label3, Some(cnd_reg))))
        } else {
            (None, try!(self.expression(*b_rhs, exit.clone(), None)))
        };
        let cnd_reg = self.register_allocator.fresh();
        let label2 = self.label_allocator.fresh();
        let label3 = self.label_allocator.fresh();
        self.instructions.insert(label2, Instruction::Branch(x64Branch::je, exit, label));
        self.instructions.insert(label3, Instruction::BinaryOp(x64BinaryOp::test, cnd_reg, cnd_reg, label2));
        let label4 = try!(self.expression(*b_lhs, label3, Some(cnd_reg)));
        if let Some(result) = result {
            let label5 = self.label_allocator.fresh();
            self.instructions.insert(label5, Instruction::Const(0, result, label4));
            Ok(label5)
        } else {
            Ok(label4)
        }
    }

    fn expr_bin_or(& mut self, b_lhs: Box<tast::Expression>, b_rhs: Box<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        let (result, label) = if let Some(result) = result_reg {
            let cnd_reg = self.register_allocator.fresh();
            let label1 = self.label_allocator.fresh();
            let label2 = self.label_allocator.fresh();
            let label3 = self.label_allocator.fresh();
            self.instructions.insert(label1, Instruction::Const(0, result, exit));
            self.instructions.insert(label2, Instruction::Branch(x64Branch::jne, exit, label1));
            self.instructions.insert(label3, Instruction::BinaryOp(x64BinaryOp::test, cnd_reg, cnd_reg, label2));
            (Some(result), try!(self.expression(*b_rhs, label3, Some(cnd_reg))))
        } else {
            (None, try!(self.expression(*b_rhs, exit.clone(), None)))
        };
        let cnd_reg = self.register_allocator.fresh();
        let label2 = self.label_allocator.fresh();
        let label3 = self.label_allocator.fresh();
        self.instructions.insert(label2, Instruction::Branch(x64Branch::jne, exit, label));
        self.instructions.insert(label3, Instruction::BinaryOp(x64BinaryOp::test, cnd_reg, cnd_reg, label2));
        let label4 = try!(self.expression(*b_lhs, label3, Some(cnd_reg)));
        if let Some(result) = result {
            let label5 = self.label_allocator.fresh();
            self.instructions.insert(label5, Instruction::Const(1, result, label4));
            Ok(label5)
        } else {
            Ok(label4)
        }
    }

    fn expr_call(& mut self, name: String, parameters: Vec<tast::Expression>, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        let result = if let Some(result) = result_reg {
            result
        } else {
            self.register_allocator.fresh()
        };
        let mut registers = Vec::new();
        let mut label : Label = self.label_allocator.fresh();
        for _ in 0..parameters.len() {
            registers.push(self.register_allocator.fresh());
        }
        self.instructions.insert(label.clone(), Instruction::Call(result, name, registers.clone(), exit));
        for (reg, parameter) in registers.into_iter().zip(parameters).rev() {
            label = try!(self.expression(parameter, label, Some(reg)));
        }
        Ok(label)
    }

    fn expression(& mut self, expr: tast::Expression, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {
        match expr.kind {
            tast::ExprKind::Const(ivalue) => {
                self.expr_const(ivalue, exit, result_reg)
            },
            tast::ExprKind::Lvalue(name) => {
                self.expr_lvalue(name, exit, result_reg)
            },
            tast::ExprKind::Sizeof(typename) => {
                self.expr_sizeof(typename, exit, result_reg)
            },
            tast::ExprKind::MembDeref(box_expr, membername) => {
                self.expr_memb_deref(box_expr, membername, exit, result_reg)
            },
            tast::ExprKind::Unary(op, box_expr) => {
                self.expr_unary(op, box_expr, exit, result_reg)
            },
            tast::ExprKind::Binary(b_lhs, op, b_rhs) => {
                match op {
                    tast::BinaryOp::Affect => {
                        self.expr_bin_affect(b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Plus => {
                        self.expr_bin_plus(b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Minus => {
                        self.expr_bin_minus(b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Mult => {
                        self.expr_bin_mult(b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Div => {
                        self.expr_bin_div(b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Equal => {
                        self.expr_bin_cmp(x64UnaryOp::sete, b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::NotEqual => {
                        self.expr_bin_cmp(x64UnaryOp::setne, b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Lower => {
                        self.expr_bin_cmp(x64UnaryOp::setl, b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::LowerEq => {
                        self.expr_bin_cmp(x64UnaryOp::setle, b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Greater => {
                        self.expr_bin_cmp(x64UnaryOp::setg, b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::GreaterEq => {
                        self.expr_bin_cmp(x64UnaryOp::setge, b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::And => {
                        self.expr_bin_and(b_lhs, b_rhs, exit, result_reg)
                    },
                    tast::BinaryOp::Or => {
                        self.expr_bin_or(b_lhs, b_rhs, exit, result_reg)
                    },
                }
            },
            tast::ExprKind::Call(name, parameters) => {
                self.expr_call(name, parameters, exit, result_reg)
            },
        }
    }


    fn find_var(& self, name: &str) -> Option<Register> {
        for scope in self.variables.iter().rev() {
            if let Some(reg) = scope.get(name) {
                return Some(*reg);
            }
        }
        None
    }
}

impl FuncDefinition {
    fn new(builder: FuncDefinitionBuilder, name: String, entry_label: Label,) -> Self{
        FuncDefinition{
            name: name,
            formals: builder.formals,
            result: builder.result,
            locals: builder.locals,
            entry: entry_label,
            exit: builder.exit,
            body: builder.instructions,
            label_allocator: builder.label_allocator,
            register_allocator: builder.register_allocator,
        }
    }

    pub fn from_typer_function(function: tast::Function, types: &HashMap<String, tast::Struct>, /*globals: &Vec<Ident>*//* Not needed, can infer that a variable is global*/) -> Result<FuncDefinition, String> {
        let mut builder = FuncDefinitionBuilder::new(&function, types);
        let exit = builder.exit.clone();
        match builder.bloc(function.blk, exit) {
            Err(e) => Err(e),
            Ok(entry_label) => {
                Ok(FuncDefinition::new(builder, function.name, entry_label))
            },
        }
    }
}
