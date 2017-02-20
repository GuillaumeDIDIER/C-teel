use RTL::rtltree::*;
use RTL::label::*;
use RTL::register::*;
use std::iter::FromIterator;
use typing::ast as tast;
use std::collections::{HashMap, HashSet};

impl File {
    pub fn from_typer_ast(typer_ast: tast::File) -> Result<File, String> {
        let mut file = File{globals: Vec::new(), functions: Vec::new()};
        file.globals = Vec::from_iter(typer_ast.variables.keys().map(|s: &String|{s.clone()}));
        for function in typer_ast.function_definitions {
            match FuncDefinition::from_typer_function(function, &typer_ast.types) {
                Ok(func_def) => {file.functions.push(func_def);},
                Err(e) => {return Err(e);},

            }
        }
        return Ok(file);

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
            self.locals.insert(var_registers[&var.0].clone());
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
                let result = self.result.clone();
                self.expression(expr, exit, Some(result))
            },
            tast::Statement::Expr(expr) => {
                self.expression(expr, exit, None)
            },
            _ => Err(String::from("Unimplemented"))
        }
    }

    fn expression(& mut self, expr: tast::Expression, exit: Label, result_reg: Option<Register>) -> Result<Label, String> {

        match expr.kind {
            tast::ExprKind::Const(ivalue) => {
                if let Some(result) = result_reg {
                    let label = self.label_allocator.fresh();
                    self.instructions.insert(label.clone(), Instruction::Const(ivalue, result, exit));
                    Ok(label)
                } else {
                    Ok(exit) // Noop !
                }
            },
            _ => Err(String::from("Unimplemented")),
        }
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
        }
    }

    pub fn from_typer_function(function: tast::Function, types: &HashMap<String, tast::Struct>) -> Result<FuncDefinition, String> {

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