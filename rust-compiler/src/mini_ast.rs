extern crate serde_json;

use self::serde_json::Value;
use std::fmt;

#[derive(Debug)]
pub enum Operator {
    Gt, Lt, Equal, Leq, Geq, Mult, Plus, Sub, Div, Mod,
}

#[derive(Debug)]
pub enum Expr {
    Binop{operator: Operator, lhs: Box<Expr>, rhs: Box<Expr>, line: u64},
    Invocation{id: String, args: Vec<Expr>, line: u64},
    Number{value: i64, line: u64},
    New{id: String, line: u64},
    Str{value: String, line: u64},
    Id{id: String, line: u64},
    Dot{left: Box<Expr>, id: String, line: u64},
    Null,
    True,
    False,
}

#[derive(Debug)]
pub enum LValue {
    Id{id: String, line: u64},
    Dot{left: Box<LValue>, id: String, line: u64},
}

#[derive(Debug)]
pub enum Stmt {
    Assignment{target: LValue, source: Expr},
    If{guard: Expr, then_case: Box<Stmt>, else_case: Box<Stmt>},
    While{guard: Expr, body: Box<Stmt>},
    Block(Vec<Stmt>),
    Return(Expr),
    Print{exp: Expr, endl: bool},
    Invocation{id: String, args: Vec<Expr>},
}

#[derive(Debug)]
pub enum MiniType {
    Integer,
    Boolean,
    Struct(String),
    FuncType(Vec<Box<MiniType>>, Box<MiniType>),
}

#[derive(Debug)]
pub struct Declaration {
    id: String,
    line: u64,
    mini_type: MiniType,
}

#[derive(Debug)]
pub struct StructDef {
    fields: Vec<Declaration>,
    id: String,
    line: u64,
}

#[derive(Debug)]
pub struct Function {
    body: Vec<Stmt>,
    declarations: Vec<Declaration>,
    functions: Vec<Function>,
    id: String,
    line: u64,
    parameters: Vec<Declaration>,
    return_type: MiniType,
}

#[derive(Debug)]
pub struct MiniAst {
    declarations: Vec<Declaration>,
    functions: Vec<Function>,
    typedefs: Vec<StructDef>,
}

impl From<Value> for MiniAst {
    fn from(val: Value) -> Self {
        let decls: Vec<Declaration> =
            val["declarations"].as_array().unwrap()
                .iter().map(Declaration::from).collect();

        let functions: Vec<Function> =
            val["functions"].as_array().unwrap()
                .iter().map(Function::from).collect();

        let typedefs: Vec<StructDef> =
            val["types"].as_array().unwrap()
                .iter().map(StructDef::from).collect();

        return MiniAst {
            declarations: decls,
            functions: functions,
            typedefs: typedefs,
        }
    }
}

impl<'a> From<&'a Value> for Function {
    fn from(val : &Value) -> Self {
        return Function {
            body:
                val["body"].as_array().unwrap()
                    .iter().map(Stmt::from).collect(),
            declarations:
                val["declarations"].as_array().unwrap()
                    .iter().map(Declaration::from).collect(),
            functions:
                val["functions"].as_array().unwrap()
                    .iter().map(Function::from).collect(),
            id: String::from(val["id"].as_str().unwrap()),
            line: val["line"].as_u64().unwrap(),
            parameters:
                val["parameters"].as_array().unwrap()
                    .iter().map(Declaration::from).collect(),
            return_type: MiniType::from(&val["type"]),
        };
    }
}

impl<'a> From<&'a Value> for Declaration {
    fn from(decl: &Value) -> Self {
        return Declaration {
            id: String::from(decl["id"].as_str().unwrap()),
            line: decl["line"].as_u64().unwrap(),
            mini_type: MiniType::from(&decl["type"]),
        } 
    }
}

impl<'a> From<&'a Value> for MiniType {
    fn from(val: &Value) -> Self {
        match val {
            &Value::String(ref s) =>
                match s.as_ref() {
                    "int" => MiniType::Integer,
                    "bool" => MiniType::Boolean,
                    _ => MiniType::Struct(s.clone()),
                },
            _ => MiniType::Struct(String::from("function todo")),
        }
    }
}

impl<'a> From<&'a Value> for StructDef {
    fn from(val: &Value) -> Self {
        return StructDef {
            fields:
                val["fields"].as_array().unwrap()
                    .iter().map(Declaration::from).collect(),
            id: String::from(val["id"].as_str().unwrap()),
            line: val["line"].as_u64().unwrap(),
        }
    }
}

impl<'a> From<&'a Value> for LValue {
    fn from(val: &Value) -> Self {
        if val["left"].is_null() {
            return LValue::Id{
                id: String::from(val["id"].as_str().unwrap()),
                line: val["line"].as_u64().unwrap(),
            }
        }
        else {
            return LValue::Dot{
                id: String::from(val["id"].as_str().unwrap()),
                left: Box::new(LValue::from(&val["left"])),
                line: val["line"].as_u64().unwrap(),
            }
        }
    }
}

impl<'a> From<&'a str> for Operator {
    fn from(op: &str) -> Self {
        return match op {
            "<=" => Operator::Leq,
            ">=" => Operator::Geq,
            "<" => Operator::Lt,
            ">" => Operator::Gt,
            "==" => Operator::Equal,
            "+" => Operator::Plus,
            "-" => Operator::Sub,
            "*" => Operator::Mult,
            "/" => Operator::Div,
            "%" => Operator::Mod,
            _ => panic!("Unsupported operator: {}", op),
        }
    }
}

impl<'a> From<&'a Value> for Expr {
    fn from(val: &Value) -> Self {
        let exp = val["exp"].as_str().unwrap();

        match exp {
            "num" => Expr::Number{
                value: val["value"].as_str().unwrap().parse::<i64>().unwrap(),
                line: val["line"].as_u64().unwrap(),
            },
            "binary" => Expr::Binop{
                operator: Operator::from(val["operator"].as_str().unwrap()),
                lhs: Box::new(Expr::from(&val["lft"])),
                rhs: Box::new(Expr::from(&val["rht"])),
                line: val["line"].as_u64().unwrap(),
            },
            "id" => Expr::Id{
                id: String::from(val["id"].as_str().unwrap()),
                line: val["line"].as_u64().unwrap(),
            },
            "invocation" => Expr::Invocation{
                id: String::from(val["id"].as_str().unwrap()),
                args: val["args"].as_array().unwrap()
                                 .iter().map(Expr::from).collect(),
                line: val["line"].as_u64().unwrap(),
            },
            "dot" => Expr::Dot{
                id: String::from(val["id"].as_str().unwrap()),
                left: Box::new(Expr::from(&val["left"])),
                line: val["line"].as_u64().unwrap(),
            },
            "new" => Expr::New{
                id: String::from(val["id"].as_str().unwrap()),
                line: val["line"].as_u64().unwrap(),
            },
            "null" => Expr::Null,
            "true" => Expr::True,
            "false" => Expr::False,
            _ => panic!("unsupported expression type: {}", exp),
        }
    }
}

impl<'a> From<&'a Value> for Stmt {
    fn from(val: &Value) -> Self {
        if val == &Value::Null {
            return Stmt::Block(vec!());
        }

        let stmt = val["stmt"].as_str().unwrap();

        match stmt {
            "return" => Stmt::Return(Expr::from(&val["exp"])),
            "assign" => Stmt::Assignment{
                target: LValue::from(&val["target"]),
                source: Expr::from(&val["source"]),
            },
            "block" => Stmt::Block(
                val["list"].as_array().unwrap()
                           .iter().map(Stmt::from).collect()
            ),
            "if" => Stmt::If{
                guard: Expr::from(&val["guard"]),
                then_case: Box::new(Stmt::from(&val["then"])),
                else_case: Box::new(Stmt::from(&val["else"])),
            },
            "while" => Stmt::While{
                guard: Expr::from(&val["guard"]),
                body: Box::new(Stmt::from(&val["body"])),
            },
            "print" => Stmt::Print{
                exp: Expr::from(&val["exp"]),
                endl: val["endl"].as_bool().unwrap(),
            },
            "invocation" => Stmt::Invocation{
                id: String::from(val["id"].as_str().unwrap()),
                args: val["args"].as_array().unwrap()
                                 .iter().map(Expr::from).collect(),
            },
            _ => panic!("Unimplemented statement {}", stmt),
        }
    }
}

fn disp_typedef(def: &StructDef, f: &mut fmt::Formatter, indent: usize) {
    write!(f, "{}Struct {} (line {}):\n", "  ".repeat(indent),
              def.id, def.line).expect("fml");
    for ref field in &def.fields { disp_decl(field, f, indent + 2) }
}

fn disp_type(mini_type: &MiniType, f: &mut fmt::Formatter, indent: usize) {
    write!(f, "{}", "  ".repeat(indent)).expect("fml");
    write!(f, "{:?}\n", mini_type).expect("fml");
}

fn disp_expr(expr: &Expr, f: &mut fmt::Formatter, indent: usize) {
    write!(f, "{}", "  ".repeat(indent)).expect("fml");
    write!(f, "{:?}\n", expr).expect("fml");
}

fn disp_decl(decl: &Declaration, f: &mut fmt::Formatter, indent: usize) {
    write!(f, "{}", "  ".repeat(indent)).expect("fml");
    write!(f, "{:?}\n", decl).expect("fml");
}

fn disp_stmt(stmt: &Stmt, f: &mut fmt::Formatter, indent: usize) {
    match stmt {
        &Stmt::Block(ref stmts) => {
            write!(f, "{}Block:\n", "  ".repeat(indent)).expect("fml");
            for ref substmt in stmts { disp_stmt(substmt, f, indent + 1) }
        }
        &Stmt::While{ref guard, ref body} => {
            write!(f, "{}While:\n", "  ".repeat(indent)).expect("fml");
            disp_expr(guard, f, indent + 1);
            write!(f, "{} Body:\n", "  ".repeat(indent)).expect("fml");
            disp_stmt(body, f, indent + 1);
        },
        &Stmt::If{ref guard, ref then_case, ref else_case} => {
            write!(f, "{}If:\n", "  ".repeat(indent)).expect("fml");
            disp_expr(guard, f, indent + 1);
            write!(f, "{} Then:\n", "  ".repeat(indent)).expect("fml");
            disp_stmt(then_case, f, indent + 1);
            write!(f, "{} Else:\n", "  ".repeat(indent)).expect("fml");
            disp_stmt(else_case, f, indent + 1);
        },
        &Stmt::Assignment{ref target, ref source} => {
            write!(f, "{}", "  ".repeat(indent)).expect("fml");
            write!(f, "assign {:?} = {:?}\n", target, source).expect("fml");
        },
        _ => {
            write!(f, "{}", "  ".repeat(indent)).expect("fml");
            write!(f, "{:?}\n", stmt).expect("fml");
        },
    }
}

fn disp_func(func: &Function, f: &mut fmt::Formatter, indent: usize) {
    write!(f, "{}{}:\n", "  ".repeat(indent), func.id).expect("fml");
    write!(f, "{}type:\n", "  ".repeat(indent + 1)).expect("fml");
    disp_type(&func.return_type, f, indent + 2);

    write!(f, "{}params:\n", "  ".repeat(indent + 1)).expect("fml");
    for ref param in &func.parameters { disp_decl(param, f, indent + 2) }
    
    write!(f, "{}decls:\n", "  ".repeat(indent + 1)).expect("fml");
    for ref decl in &func.declarations { disp_decl(decl, f, indent + 2) }

    write!(f, "{}body:\n", "  ".repeat(indent + 1)).expect("fml");
    for ref stmt in &func.body { disp_stmt(stmt, f, indent + 2) }

    write!(f, "{}functions:\n", "  ".repeat(indent + 1)).expect("fml");
    for ref func in &func.functions { disp_func(func, f, indent + 2) }
}

fn disp(ast: &MiniAst, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "MiniAst:\n  Types:\n").expect("fml");
    for ref typ in &ast.typedefs { disp_typedef(typ, f, 2) }
    write!(f, "  Decls:\n").expect("fml");
    for ref decl in &ast.declarations { disp_decl(decl, f, 2) }
    write!(f, "  Funcs:\n").expect("fml");
    for ref func in &ast.functions { disp_func(func, f, 2) }
    write!(f, "\n")
}

impl fmt::Display for MiniAst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        disp(self, f)
    }
}

pub fn generate_ast(source: &str) -> MiniAst {
    println!("{}", source);
    let v: Value = match serde_json::from_str(source) {
        Err(why) => panic!(why),
        Ok(value) => value,
    };

    return MiniAst::from(v);
}
