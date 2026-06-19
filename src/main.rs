use pest::iterators::Pair;
use parser::{MiniParser, Rule};
use std::fs;

mod parser;

/// 单递归函数：输入 pest AST 节点，输出对应 Rust 源码字符串
fn generate_rust(pair: Pair<Rule>) -> String {
    match pair.as_rule() {
        Rule::file => {
            let mut buf = String::new();
            for inner in pair.into_inner() {
                buf.push_str(&generate_rust(inner));
            }
            buf
        }

        Rule::top_stmt => {
            let mut buf = String::new();
            for inner in pair.into_inner() {
                buf.push_str(&generate_rust(inner));
            }
            buf
        }

        // struct User { id:u64 name:String }
        Rule::struct_def => {
            let mut inner = pair.into_inner();
            let _struct_kw = inner.next().unwrap();
            let name = generate_rust(inner.next().unwrap());
            let mut fields = String::new();
            for field in inner {
                fields.push_str(&generate_rust(field));
            }
            format!(
                "#[derive(Debug, Clone)]\nstruct {} {{\n{}}}\n",
                name, fields
            )
        }
        Rule::struct_field => {
            let mut inner = pair.into_inner();
            let id = generate_rust(inner.next().unwrap());
            let _colon = inner.next().unwrap();
            let ty = generate_rust(inner.next().unwrap());
            format!("    {}: {},\n", id, ty)
        }

        // fn 函数定义
        Rule::func_def => {
            let mut inner = pair.into_inner();
            let _fn_kw = inner.next().unwrap();
            let fn_name = generate_rust(inner.next().unwrap());
            let params = generate_rust(inner.next().unwrap());
            let ret_sig_node = inner.next().unwrap();
            let block = generate_rust(inner.next().unwrap());

            let ret_raw = generate_rust(ret_sig_node);
            let ret_ty = if ret_raw.is_empty() {
                "()".to_string()
            } else {
                ret_raw
            };

            format!("fn {}{} -> {} {}\n", fn_name, params, ret_ty, block)
        }
        Rule::param_list => {
            let mut buf = String::new();
            let mut inner = pair.into_inner();
            while let Some(p) = inner.next() {
                buf.push_str(&generate_rust(p));
                if inner.peek().is_some() {
                    buf.push_str(", ");
                }
            }
            format!("({})", buf)
        }
        Rule::param => {
            let mut inner = pair.into_inner();
            let id = generate_rust(inner.next().unwrap());
            let _colon = inner.next().unwrap();
            let ty = generate_rust(inner.next().unwrap());
            format!("{}: {}", id, ty)
        }
        Rule::ret_sig => {
            let mut inner = pair.into_inner();
            if let Some(_arrow) = inner.next() {
                let ty = generate_rust(inner.next().unwrap());
                ty
            } else {
                String::new()
            }
        }

        // 语句块 { ... }
        Rule::block => {
            let mut buf = String::new();
            for stmt in pair.into_inner() {
                buf.push_str(&generate_rust(stmt));
            }
            format!("{{\n{}}}", buf)
        }

        // let / mut 变量定义
        Rule::let_stmt => {
            let mut inner = pair.into_inner();
            let mut is_mut = false;
            let mut var_name = String::new();
            let mut type_anno = String::new();
            let mut expr_val = String::new();

            while let Some(n) = inner.next() {
                let s = n.as_str();
                if s == "mut" && var_name.is_empty() {
                    is_mut = true;
                } else if var_name.is_empty() {
                    var_name = generate_rust(n);
                } else if s == ":" {
                    type_anno = format!(": {}", generate_rust(inner.next().unwrap()));
                } else if s == "=" {
                    expr_val = generate_rust(inner.next().unwrap());
                }
            }

            let mut_kw = if is_mut { "mut " } else { "" };
            format!("let {}{}{} = {};\n", mut_kw, var_name, type_anno, expr_val)
        }

        // ret expr → return expr
        Rule::ret_stmt => {
            let expr = generate_rust(pair.into_inner().next().unwrap());
            format!("return {};\n", expr)
        }

        // print(xxx) → println!("{}", xxx)
        Rule::print_stmt => {
            let mut inner = pair.into_inner();
            let _print = inner.next();
            let _lparen = inner.next();
            let expr = generate_rust(inner.next().unwrap());
            let _rparen = inner.next();
            format!("println!(\"{{}}\", {});\n", expr)
        }

        // if / else
        Rule::if_stmt => {
            let mut inner = pair.into_inner();
            let _if = inner.next();
            let cond = generate_rust(inner.next().unwrap());
            let block = generate_rust(inner.next().unwrap());
            let mut else_block = String::new();
            if let Some(else_node) = inner.next() {
                else_block = format!(" else {}", generate_rust(else_node));
            }
            format!("if {} {}{}\n", cond, block, else_block)
        }
        Rule::for_stmt => {
            let mut inner = pair.into_inner();
            let _for = inner.next();
            let var = generate_rust(inner.next().unwrap());
            let _in = inner.next();
            let expr = generate_rust(inner.next().unwrap());
            let block = generate_rust(inner.next().unwrap());
            format!("for {} in {} {}\n", var, expr, block)
        }
        Rule::while_stmt => {
            let mut inner = pair.into_inner();
            let _while = inner.next();
            let cond = generate_rust(inner.next().unwrap());
            let block = generate_rust(inner.next().unwrap());
            format!("while {} {}\n", cond, block)
        }

        // 裸表达式语句
        Rule::expr_stmt => {
            let expr = generate_rust(pair.into_inner().next().unwrap());
            format!("{};\n", expr)
        }

        // 表达式（优先级层级，全部通过兜底展开）
        Rule::expr | Rule::add_expr | Rule::mul_expr | Rule::cmp_expr => {
            let mut inner = pair.into_inner();
            let left = generate_rust(inner.next().unwrap());
            let mut buf = left;
            while let Some(op) = inner.next() {
                buf.push_str(" ");
                buf.push_str(&generate_rust(op));
                if let Some(right) = inner.next() {
                    buf.push_str(" ");
                    buf.push_str(&generate_rust(right));
                }
            }
            buf
        }
        Rule::add_op | Rule::mul_op | Rule::cmp_op => pair.as_str().to_string(),
        Rule::primary => {
            let mut buf = String::new();
            for inner in pair.into_inner() {
                buf.push_str(&generate_rust(inner));
            }
            buf
        }
        Rule::ty_ident => pair.as_str().to_string(),
        Rule::ident => pair.as_str().to_string(),
        Rule::number => pair.as_str().to_string(),
        Rule::string => pair.as_str().to_string(),
        // 裸函数调用作为语句
        Rule::call_expr_stmt => {
            let inner = pair.into_inner().next().unwrap();
            format!("{};\n", generate_rust(inner))
        }

        Rule::call_expr => {
            let mut inner = pair.into_inner();
            let fn_name = generate_rust(inner.next().unwrap());
            let _lparen = inner.next();
            let args = inner.next().map(|a| generate_rust(a)).unwrap_or_default();
            let _rparen = inner.next();
            format!("{}({})", fn_name, args)
        }
        Rule::call_args => {
            let mut buf = String::new();
            let mut inner = pair.into_inner();
            while let Some(arg) = inner.next() {
                buf.push_str(&generate_rust(arg));
                if inner.peek().is_some() {
                    buf.push_str(", ");
                }
            }
            buf
        }

        // 注释和空白直接丢弃
        Rule::COMMENT | Rule::WHITESPACE | Rule::NEWLINE => String::new(),

        // 兜底
        _ => {
            let mut buf = String::new();
            for inner in pair.into_inner() {
                buf.push_str(&generate_rust(inner));
            }
            buf
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let src = fs::read_to_string("test.mini")?;
    let parsed = MiniParser::parse(Rule::file, &src)?;
    let rust_code = generate_rust(parsed.next().unwrap());

    fs::write("output.rs", &rust_code)?;
    println!("=== Generated Rust Code (output.rs) ===");
    println!("{}", rust_code);
    Ok(())
}
