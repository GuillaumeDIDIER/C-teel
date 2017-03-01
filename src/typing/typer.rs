//use parse;
use parse::ast as past;
use std::collections::{HashMap, HashSet};
use typing::ast as tast;


impl tast::File {
    pub fn type_file(src_ast: Vec<past::Node<past::Declaration>>) -> Result<tast::File, String> {
        let mut file = tast::File{
            variables: HashMap::new(),
            function_declarations: HashMap::new(),
            types: HashMap::new(),
            function_definitions: Vec::new(),
        };
        let mut f_sbrk = tast::FunctionProto{
            ret_type: tast::Type::Void,
            name: String::from("sbrk"),
            params_type: Vec::new(),
        };
        f_sbrk.params_type.push(tast::Type::Int);

        let mut f_putchar = tast::FunctionProto{
            ret_type: tast::Type::Int,
            name: String::from("putchar"),
            params_type: Vec::new(),
        };
        f_putchar.params_type.push(tast::Type::Int);

        file.function_declarations.insert(f_sbrk.name.clone(), f_sbrk);
        file.function_declarations.insert(f_putchar.name.clone(), f_putchar);

        for decl in src_ast {
            match decl.t {
                past::Declaration::Func(nf) => {
                    // 1. Get function proto, and insert it into the symbol map.
                    let (typ, name, params, blk) = match nf.t {
                        past::DeclFunc::Int(n_name, params, n_blk) => (tast::Type::Int, n_name.t, params, n_blk.t),
                        past::DeclFunc::Struct(n_type, n_name, params, n_blk) => {
                            if file.types.contains_key(&n_type.t) {
                                (tast::Type::Struct(n_type.t), n_name.t, params, n_blk.t)
                            } else {
                                return Err(String::from("Unkown struct type"));
                            }
                        }

                    };
                    match type_parameters(&params, &file.types) {
                        Ok(typed_params) => {
                            let proto = tast::FunctionProto{ret_type: typ.clone(), name: name.clone(), params_type: typed_params};
                            if file.function_declarations.contains_key(&proto.name) {
                                return Err(String::from("Function redefinition"));
                            }
                            file.function_declarations.insert(proto.name.clone(), proto);
                        },
                        Err(e) => {
                            return Err(e);
                        }
                    }

                    // 2. Type the function itself.
                    let res = tast::Function::type_function(typ, name, params, blk, &file.types, &file.function_declarations, &file.variables);
                    match res {
                        Ok(f) => {
                            file.function_definitions.push(f)
                        },
                        Err(e) => {
                            return Err(e);
                        }
                    }



                },
                past::Declaration::Var(nv) => { // Case content should be factored into a function.
                    let vars = tast::Var::type_declaration(nv.t, &file.types);
                    match vars {
                        Err(e) => {
                            return Err(e);
                        },
                        Ok(vars) => {
                            for v in vars {
                                if file.variables.contains_key(&v.name) {
                                    return Err(String::from("global variable redefined"))
                                } else {
                                    file.variables.insert(v.name.clone(), v);
                                }

                            }
                        }

                    }
                },
                past::Declaration::Type(nt) => {
                    let typ = nt.t;
                    let name = typ.0.t;
                    if file.types.contains_key(&name) {
                        return Err(String::from("Struct type redefinition"));
                    }
                    let r = tast::Struct::new(&name, typ.1, &file.types);
                    match r {
                        Err(e) => {
                            return Err(e);
                        },
                        Ok(s) => {
                            file.types.insert(name, s);
                        }
                    }
                }
            }
        };
        {
            let varnames : HashSet<&String> = file.variables.keys().collect();
            let funcnames : HashSet<&String> = file.function_declarations.keys().collect();
            if varnames.intersection(&funcnames).count() > 0 {
                return Err(String::from("Duplicate definition of global symbol (global variable or function)"));
            }
        }
        if let Some(main_func) = file.function_declarations.get("main"){
            if !main_func.ret_type.compatible(&tast::Type::Int) {
                return Err(String::from("Wrong return type for main entry point"));
            }
        } else {
            return Err(String::from("Missing main entry point"));
        }
        Ok(file)
    }
}

impl tast::Struct {
    fn new(name: &String,
        members: Vec<past::Node<past::DeclVar>>,
        types: &HashMap<String, tast::Struct>
    ) -> Result<tast::Struct, String> {
        let mut s = tast::Struct{members: Vec::new(), index: HashMap::new()};
        let mut types = types.clone();
        types.insert(name.clone(), s.clone()); // This enables reference to the struct itself as a pointer in fields.
        for n_member in members {
            let vars = tast::Var::type_declaration(n_member.t, &types);
            match vars {
                Err(e) => {
                    return Err(e);
                },
                Ok(vars) => {
                    for v in vars {
                        if s.index.contains_key(&v.name) {
                            return Err(String::from("struct member redefined"))
                        } else {
                            s.index.insert(v.name.clone(), s.members.len());
                            s.members.push(v);
                        }

                    }
                }

            }

        }
        Ok(s)
    }
}

impl tast::Var {
    fn type_declaration(src: past::DeclVar, types: &HashMap<String, tast::Struct>) -> Result<Vec<tast::Var>, String> {
        match src {
            past::DeclVar::Int(names) => {
                let mut vars = Vec::new();
                for n_name in names {
                    vars.push(tast::Var::new(tast::Type::Int, n_name.t))
                }
                Ok(vars)
            },
            past::DeclVar::Struct(n_type_name, names) => {
                if types.contains_key(&n_type_name.t) {
                    let mut vars = Vec::new();
                    for n_name in names {
                        vars.push(tast::Var::new(tast::Type::Struct(n_type_name.t.clone()), n_name.t))
                    }
                    Ok(vars)

                } else {
                    Err(String::from("Undefined Type"))
                }
            }
        }
    }

    fn new(typ: tast::Type, name: String) -> tast::Var {
        tast::Var{
            typ: typ,
            name: name,
        }
    }
}

fn type_parameters(
    params: &Vec<past::Node<past::Param>>,
    types: &HashMap<String, tast::Struct>
) -> Result<Vec<tast::Type>, String> {
    let mut res = Vec::new();
    for n_param in params {
        match n_param.t {
            past::Param::Int(_) => {res.push(tast::Type::Int);},
            past::Param::Struct(ref typ, _) => {
                if types.contains_key(&typ.t){
                    res.push(tast::Type::Struct(typ.t.clone()))
                } else {
                    return Err(String::from("Undefined parameter type"));
                }
            },
        }
    }
    Ok(res)
}

impl tast::Function {
    fn type_function(typ: tast::Type, name: String,
        params: Vec<past::Node<past::Param>>, blk: past::Bloc,
        types: &HashMap<String, tast::Struct>,
        symbols: &HashMap<String, tast::FunctionProto>,
        globals: &HashMap<String, tast::Var>
    ) -> Result<tast::Function,String> {
        let mut params_name = Vec::new();
        let mut vars = HashMap::new();
        for p in params {
            match p.t {
                past::Param::Int(name) => {
                    if vars.contains_key(&name.t) {
                        return Err(String::from("Duplicate parameter name"))
                    }
                    let v = tast::Var::new(tast::Type::Int, name.t.clone());
                    vars.insert(name.t.clone(), v);
                    params_name.push(name.t);
                },
                past::Param::Struct(typ, name) => {
                    if types.contains_key(&typ.t){
                        if vars.contains_key(&name.t) {
                            return Err(String::from("Duplicate parameter name"))
                        }
                        let v = tast::Var::new(tast::Type::Struct(typ.t), name.t.clone());
                        vars.insert(name.t.clone(), v);
                        params_name.push(name.t);
                    } else {
                        return Err(String::from("Undefined parameter type"));
                    }
                }
            }
        }
        let typed_bloc = match tast::Bloc::type_bloc(blk, types, symbols, &vec![globals, &vars], &typ) {
            Ok(blk) => blk,
            Err(e) => {return Err(e);}
        };
        Ok(tast::Function{
            ret_type: typ,
            name: name,
            params: params_name,
            vars: vars,
            blk: typed_bloc,
        })
    }
}

impl tast::Bloc {
    fn type_bloc(bloc: past::Bloc,
        types: &HashMap<String, tast::Struct>,
        symbols: &HashMap<String, tast::FunctionProto>,
        globals: &Vec<&HashMap<String, tast::Var>>,
        ret_type: &tast::Type,
    ) -> Result<tast::Bloc, String> {
        let (decls, stmts) = bloc;
        // 1 type declarations,
        let mut typed_decls = HashMap::new();
        for decl in decls { // Loop content should be factored into a function.
            let vars = tast::Var::type_declaration(decl.t, &types);
            match vars {
                Err(e) => {
                    return Err(e);
                },
                Ok(vars) => {
                    for v in vars {
                        if typed_decls.contains_key(&v.name) {
                            return Err(String::from("variable redefined"))
                        } else {
                            typed_decls.insert(v.name.clone(), v);
                        }

                    }
                }
            }
        }

        // 2 type statements.
        let mut typed_stmts = Vec::new();
        {
            let mut stack = globals.clone();
            stack.push(&typed_decls);
            for node_stmt in stmts {
                match tast::Statement::type_statement(node_stmt.t, types, symbols, &stack, &ret_type){
                    Err(e) => {return Err(e)},
                    Ok(stmt) => {typed_stmts.push(stmt);},
                }
            }
        }

        Ok(tast::Bloc{
            decls: typed_decls,
            stmts: typed_stmts,
        })
    }
}

impl tast::Statement { // Fixme : return types !!!
    fn type_statement(stmt: past::Statement,
        types: &HashMap<String, tast::Struct>,
        symbols: &HashMap<String, tast::FunctionProto>,
        vars: &Vec<&HashMap<String, tast::Var>>,
        ret_type: &tast::Type,
    ) -> Result<tast::Statement, String> {
        match stmt {
            past::Statement::Expr(node_expr) => {
                match tast::Expression::type_expression(node_expr.t, types, symbols, vars) {
                    Ok(expr) => Ok(tast::Statement::Expr(expr)),
                    Err(e) => Err(e),
                }
            },
            past::Statement::If(node_condition, b_n_stmt) => {
                match tast::Expression::type_expression(node_condition.t, types, symbols, vars) {
                    Ok(cond) => {
                        match tast::Statement::type_statement(b_n_stmt.t, types, symbols, vars, ret_type) {
                            Ok(stmt) => Ok(tast::Statement::If(cond, Box::new(stmt))),
                            Err(e) => Err(e),
                        }
                    },
                    Err(e) => Err(e),
                }
            },
            past::Statement::IfElse(node_condition, b_n_stmt, b_n_stmt_else) => {
                match tast::Expression::type_expression(node_condition.t, types, symbols, vars) {
                    Ok(cond) => {
                        match tast::Statement::type_statement(b_n_stmt.t, types, symbols, vars, ret_type) {
                            Ok(stmt) => {
                                match tast::Statement::type_statement(b_n_stmt_else.t, types, symbols, vars, ret_type) {
                                    Ok(stmt_else) => Ok(tast::Statement::IfElse(cond, Box::new(stmt), Box::new(stmt_else))),
                                    Err(e) => Err(e),
                                }
                            }
                            Err(e) => Err(e),
                        }
                    },
                    Err(e) => Err(e),
                }
            },
            past::Statement::While(node_condition, b_n_stmt) => {
                match tast::Expression::type_expression(node_condition.t, types, symbols, vars) {
                    Ok(cond) => {
                        match tast::Statement::type_statement(b_n_stmt.t, types, symbols, vars, ret_type) {
                            Ok(stmt) => Ok(tast::Statement::While(cond, Box::new(stmt))),
                            Err(e) => Err(e),
                        }
                    },
                    Err(e) => Err(e),
                }
            },
            past::Statement::Return(node_expr) => {
                match tast::Expression::type_expression(node_expr.t, types, symbols, vars) {
                    Ok(expr) => {
                        if expr.typ.compatible(ret_type) {
                            Ok(tast::Statement::Return(expr))
                        } else {
                            Err(String::from("Incompatible return type"))
                        }},
                    Err(e) => Err(e),
                }
            },
            past::Statement::Bloc(node_bloc) => {

                match tast::Bloc::type_bloc(node_bloc.t, types, symbols, vars, ret_type) {
                    Err(e) => Err(e),
                    Ok(bloc) => Ok(tast::Statement::Bloc(bloc)),
                }

            },
            past::Statement::Noop => {
                Ok(tast::Statement::Noop)
            }
        }
    }
}

impl tast::ExprKind {
    pub fn lvalue(&self) -> bool {
        match self {
            &tast::ExprKind::Lvalue(_) |  &tast::ExprKind::MembDeref(..) => true,
            _ => false,
        }
    }
}

impl tast::Expression {
    fn type_expression(expr: past::Expression,
        types: &HashMap<String, tast::Struct>,
        symbols: &HashMap<String, tast::FunctionProto>,
        vars: &Vec<&HashMap<String, tast::Var>>,
    ) -> Result<tast::Expression, String> {
        match expr {
            past::Expression::Int(value) => {
                if value == 0 {
                    Ok(tast::Expression::new(tast::Type::Null, tast::ExprKind::Const(value)))
                } else {
                    Ok(tast::Expression::new(tast::Type::Int, tast::ExprKind::Const(value)))
                }
            },
            past::Expression::Ident(node_ident) => {
                for scope in vars.iter().rev() {
                    if let Some(v) = scope.get(&node_ident.t) {
                        return Ok(tast::Expression::new(
                            v.typ.clone(),
                            tast::ExprKind::Lvalue(v.name.clone())
                        ));
                    }
                }
                Err(String::from("Unknown variable"))
            },
            past::Expression::MembDeref(b_n_expr, node_ident) => {
                match tast::Expression::type_expression(b_n_expr.t, types, symbols, vars) {
                    Err(e) => Err(e),
                    Ok(expr) => {
                        if let tast::Type::Struct(s) = expr.typ.clone() {
                            if let Some(typ) = types.get(&s) {
                                if let Some(i) = typ.index.get(&node_ident.t) {
                                    Ok(tast::Expression::new(
                                        typ.members[i.clone()].typ.clone(),
                                        tast::ExprKind::MembDeref(Box::new(expr), node_ident.t)
                                    ))
                                } else {
                                    Err(String::from("Non existent field in structure"))
                                }
                            } else {
                                Err(String::from("Unkown struct type"))
                            }

                        } else {
                            Err(String::from("Trying to dereference non struct variable"))
                        }
                    },
                }
            },
            past::Expression::Call(node_ident, parameters) => { // fixme !
                if let Some(function) = symbols.get(&node_ident.t) {
                    let mut typed_parameters = Vec::new();
                    if function.params_type.len() != parameters.len() {
                        return Err(String::from("Wrong number of parameters"));
                    };
                    for (param_type, node_param) in function.params_type.iter().zip(parameters) {
                        match tast::Expression::type_expression(node_param.t, types, symbols, vars) {
                            Err(e) => return Err(e),
                            Ok(expr) => {
                                if expr.typ.compatible(param_type) {
                                    typed_parameters.push(expr);
                                } else {
                                    return Err(String::from("Mismatched type"));
                                }
                            }
                        }

                    };

                    Ok(tast::Expression::new(
                        function.ret_type.clone(),
                        tast::ExprKind::Call(node_ident.t, typed_parameters)

                    ))
                } else {
                    Err(String::from("Unknown function"))
                }
            },
            past::Expression::Unary(node_unary_op, b_n_expr) => {
                match node_unary_op.t {
                    tast::UnaryOp::Minus => {
                        match tast::Expression::type_expression(b_n_expr.t, types, symbols, vars) {
                            Ok(expr) => {
                                if tast::Type::Int.compatible(&expr.typ) {
                                    Ok(tast::Expression::new(
                                        tast::Type::Int,
                                        tast::ExprKind::Unary(tast::UnaryOp::Minus, Box::new(expr))
                                    ))
                                } else {
                                    Err(String::from("Wrongly typed operand for minus: expected int"))
                                }
                            },
                            Err(e) => Err(e)
                        }
                    },
                    tast::UnaryOp::Not => {
                        match tast::Expression::type_expression(b_n_expr.t, types, symbols, vars) {
                            Ok(expr) => Ok(tast::Expression::new(
                                tast::Type::Int,
                                tast::ExprKind::Unary(tast::UnaryOp::Not, Box::new(expr))
                            )),
                            Err(e) => Err(e)
                        }
                    }
                }
            },
            past::Expression::Binary(b_n_lhs, node_binary_op, b_n_rhs) => {
                let lhs_res = tast::Expression::type_expression(b_n_lhs.t, types, symbols, vars);
                let rhs_res = tast::Expression::type_expression(b_n_rhs.t, types, symbols, vars);
                match (lhs_res, rhs_res) {
                    (Err(e), _) | (Ok(_), Err(e)) => Err(e),
                    (Ok(lhs), Ok(rhs)) => {
                        match node_binary_op.t {
                            e @ tast::BinaryOp::Or
                            | e @ tast::BinaryOp::And => {
                                Ok(tast::Expression::new(
                                    tast::Type::Int,
                                    tast::ExprKind::Binary(Box::new(lhs), e, Box::new(rhs))
                                ))
                            },
                            e @ tast::BinaryOp::Equal
                            | e @ tast::BinaryOp::NotEqual
                            | e @ tast::BinaryOp::Lower
                            | e @ tast::BinaryOp::LowerEq
                            | e @ tast::BinaryOp::Greater
                            | e @ tast::BinaryOp::GreaterEq => {
                                if lhs.typ.compatible(&rhs.typ) {
                                    Ok(tast::Expression::new(
                                        tast::Type::Int,
                                        tast::ExprKind::Binary(Box::new(lhs), e, Box::new(rhs))
                                    ))
                                } else {
                                    Err(String::from("Comparing incompatible type"))
                                }
                            },
                            e @ tast::BinaryOp::Plus
                            | e @ tast::BinaryOp::Minus
                            | e @ tast::BinaryOp::Mult
                            | e @ tast::BinaryOp::Div => {
                                if lhs.typ.compatible(&tast::Type::Int) && rhs.typ.compatible(&tast::Type::Int) {
                                    Ok(tast::Expression::new(
                                        tast::Type::Int,
                                        tast::ExprKind::Binary(Box::new(lhs), e, Box::new(rhs))
                                    ))
                                } else {
                                    Err(String::from("Trying to do arithmetic operation on non arithmetic type"))
                                }
                            }
                            tast::BinaryOp::Affect => {
                                if lhs.kind.lvalue() {
                                    if lhs.typ.compatible(&rhs.typ) {
                                        Ok(tast::Expression::new(
                                            lhs.typ.clone(),
                                            tast::ExprKind::Binary(Box::new(lhs), tast::BinaryOp::Affect, Box::new(rhs))
                                        ))
                                    } else {
                                        Err(String::from("Affectation of incompatible type"))
                                    }
                                } else {
                                    Err(String::from("Trying to affect to a non lvalue"))
                                }

                            }
                        }
                    },
                }

            },
            past::Expression::Sizeof(node_ident) => {
                if let Some(_) =  types.get(&node_ident.t){
                    Ok(tast::Expression::new(
                        tast::Type::Int,
                        tast::ExprKind::Sizeof(node_ident.t)
                    ))
                } else {
                    Err(String::from("Sizeof unknown type"))
                }
            },
            past::Expression::Parens(b_n_expr) => tast::Expression::type_expression(b_n_expr.t, types, symbols, vars),
        }
    }
}

impl tast::Type {
    fn compatible(&self, other: &Self) -> bool {
        match (self, other) {
            (&tast::Type::Int , &tast::Type::Int)
            | (&tast::Type::Int , &tast::Type::Null)
            | (&tast::Type::Null , &tast::Type::Int)
            | (&tast::Type::Null , &tast::Type::Null)
            | (&tast::Type::Struct(_), &tast::Type::Null)
            | (&tast::Type::Struct(_), &tast::Type::Void)
            | (&tast::Type::Null, &tast::Type::Struct(_))
            | (&tast::Type::Void, &tast::Type::Struct(_))
            | (&tast::Type::Void, &tast::Type::Void) => true,
            (&tast::Type::Struct(ref s1), &tast::Type::Struct(ref s2)) => (s1 == s2),
            _ => false,
        }
    }
}
