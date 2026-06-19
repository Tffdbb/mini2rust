use pest::{iterators::Pair, Parser};
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
            let s = pair.as_str().trim();
            if s.starts_with("->") {
                s[2..].trim().to_string()
            } else {
                s.to_string()
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
            let first = inner.next().unwrap();
            let ident_node = if first.as_str() == "mut" {
                is_mut = true;
                inner.next().unwrap()
            } else {
                first
            };
            let var_name = generate_rust(ident_node);
            let mut type_anno = String::new();

            // 处理 :ty 和 =
            let expr_val = if let Some(maybe_colon) = inner.peek() {
                if maybe_colon.as_str() == ":" {
                    inner.next(); // skip :
                    let ty = generate_rust(inner.next().unwrap());
                    type_anno = format!(": {}", ty);
                    inner.next(); // skip =
                    generate_rust(inner.next().unwrap())
                } else {
                    inner.next(); // skip =
                    generate_rust(inner.next().unwrap())
                }
            } else {
                String::new()
            };

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
            let expr = generate_rust(pair.into_inner().next().unwrap());
            format!("println!(\"{}\", {});\n", expr, expr)
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

        // 表达式
        Rule::expr => {
            let mut buf = String::new();
            for inner in pair.into_inner() {
                buf.push_str(&generate_rust(inner));
            }
            buf
        }
        Rule::bin_expr => {
            let mut inner = pair.into_inner();
            let left = generate_rust(inner.next().unwrap());
            let op = generate_rust(inner.next().unwrap());
            let right = generate_rust(inner.next().unwrap());
            format!("{} {} {}", left, op, right)
        }
        Rule::op => pair.as_str().to_string(),
        Rule::path_expr | Rule::ty_ident => pair.as_str().to_string(),
        Rule::ident => pair.as_str().to_string(),
        Rule::number => pair.as_str().to_string(),
        Rule::string => pair.as_str().to_string(),
        Rule::call_expr => {
            let mut inner = pair.into_inner();
            let fn_name = generate_rust(inner.next().unwrap());
            let mut args = String::new();
            while let Some(arg) = inner.next() {
                args.push_str(&generate_rust(arg));
                if inner.peek().is_some() {
                    args.push_str(", ");
                }
            }
            format!("{}({})", fn_name, args)
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
