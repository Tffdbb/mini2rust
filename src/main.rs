use pest::{iterators::Pair, Parser};
use parser::{MiniParser, Rule};
use std::fs;

mod parser;

/// 单递归函数：输入 pest AST 节点，输出对应 Rust 源码字符串
fn generate_rust(pair: Pair<Rule>) -> String {
    match pair.as_rule() {
        Rule::file => {
            let mut funcs = String::new();
            let mut main_stmts = String::new();
            for inner in pair.into_inner() {
                let rule = inner.as_rule();
                let code = generate_rust(inner);
                if rule == Rule::func_def || rule == Rule::struct_def {
                    funcs.push_str(&code);
                } else if !code.trim().is_empty() {
                    main_stmts.push_str(&code);
                }
            }
            if !main_stmts.is_empty() {
                funcs.push_str(&format!("fn main() {{\n{}}}\n", main_stmts));
            }
            funcs
        }

        Rule::top_stmt => {
            let mut buf = String::new();
            for inner in pair.into_inner() {
                buf.push_str(&generate_rust(inner));
            }
            buf
        }

        // struct User { id:u64 name:String }
        // NOTE: pest drops string literals from into_inner()
        // into_inner() yields: ident, struct_field*
        Rule::struct_def => {
            let mut inner = pair.into_inner();
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
            let ty = generate_rust(inner.next().unwrap());
            format!("    {}: {},\n", id, ty)
        }

        // fn 函数定义
        Rule::func_def => {
            let mut inner = pair.into_inner();
            // NOTE: pest does NOT produce sub-nodes for string literals like "fn"
            // So into_inner() yields: ident, param_list, ret_sig, block
            let fn_name = generate_rust(inner.next().unwrap());
            let params = generate_rust(inner.next().unwrap());
            
            let mut ret_raw = String::new();
            let mut block_parts = Vec::new();
            for node in inner {
                if node.as_rule() == Rule::ret_sig {
                    let r = generate_rust(node);
                    if !r.is_empty() { ret_raw = r; }
                } else {
                    block_parts.push(generate_rust(node));
                }
            }
            
            let ret_ty = if ret_raw.is_empty() {
                "()".to_string()
            } else {
                ret_raw
            };
            
            let block_str = block_parts.concat();
            format!("fn {}{} -> {} {}\n", fn_name, params, ret_ty, block_str)
        }
        Rule::inner_params => {
            let mut buf = String::new();
            let mut first = true;
            for p in pair.into_inner() {
                if !first { buf.push_str(", "); }
                first = false;
                buf.push_str(&generate_rust(p));
            }
            buf
        }
        Rule::param_list => {
            let inner = generate_rust(pair.into_inner().next().unwrap());
            format!("({})", inner)
        }
        Rule::param => {
            let id_str = pair.as_str(); // debug: full param string
            if id_str.is_empty() {
                return String::new();
            }
            let mut inner = pair.into_inner();
            let first = inner.next();
            if first.is_none() {
                return id_str.to_string();
            }
            let id = generate_rust(first.unwrap());
            // colon literal is NOT a child node, so next child is ty directly
            if let Some(ty_pair) = inner.next() {
                let ty = generate_rust(ty_pair);
                format!("{}: {}", id, ty)
            } else {
                id
            }
        }
        Rule::ret_sig => {
            let inner = pair.into_inner();
            let mut result = String::new();
            for node in inner {
                let s = node.as_str();
                if s != "->" {
                    result = generate_rust(node);
                }
            }
            result
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
            // pest drops keywords, punct from into_inner()
            // Children: ident, (opt)ty_ident, expr
            let nodes: Vec<_> = pair.clone().into_inner().collect();
            let var_name = generate_rust(nodes[0].clone());
            let expr_val = if nodes.len() >= 3 {
                generate_rust(nodes[2].clone())
            } else {
                generate_rust(nodes[1].clone())
            };
            let type_anno = if nodes.len() >= 3 {
                format!(": {}", generate_rust(nodes[1].clone()))
            } else {
                String::new()
            };
            // TODO: detect "mut" from source string
            let is_mut = pair.as_str().starts_with("mut");
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
            let mut parts = Vec::new();
            for node in pair.into_inner() {
                let s = node.as_str();
                if s != "print" && s != "(" && s != ")" {
                    parts.push(generate_rust(node));
                }
            }
            let expr = parts.concat();
            format!("println!(\"{{}}\", {});\n", expr)
        }
        Rule::print_expr => {
            let mut buf = String::new();
            for inner in pair.into_inner() {
                buf.push_str(&generate_rust(inner));
            }
            buf
        }

        // if / else
        Rule::if_stmt => {
            let mut cond = String::new();
            let mut block_parts = Vec::new();
            let mut is_else = false;
            for node in pair.into_inner() {
                let s = node.as_str();
                if s == "if" { continue; }
                if s == "else" { is_else = true; continue; }
                if node.as_rule() == Rule::block {
                    block_parts.push(generate_rust(node));
                } else if is_else {
                    block_parts.push(format!(" else {}", generate_rust(node)));
                } else if cond.is_empty() {
                    cond = generate_rust(node);
                }
            }
            format!("if {} {}\n", cond, block_parts.concat())
        }
        Rule::for_stmt => {
            let mut var = String::new();
            let mut iter_expr = String::new();
            let mut block = String::new();
            for node in pair.into_inner() {
                let s = node.as_str();
                if s == "for" || s == "in" { continue; }
                if node.as_rule() == Rule::block {
                    block = generate_rust(node);
                } else if var.is_empty() {
                    var = generate_rust(node);
                } else {
                    iter_expr = generate_rust(node);
                }
            }
            format!("for {} in {} {}\n", var, iter_expr, block)
        }
        Rule::while_stmt => {
            let mut cond = String::new();
            let mut block = String::new();
            for node in pair.into_inner() {
                let s = node.as_str();
                if s == "while" { continue; }
                if node.as_rule() == Rule::block {
                    block = generate_rust(node);
                } else if cond.is_empty() {
                    cond = generate_rust(node);
                }
            }
            format!("while {} {}\n", cond, block)
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
        Rule::name => pair.as_str().to_string(),
        Rule::ty => pair.as_str().to_string(),
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
    #[allow(unused_mut)]
    let mut parsed = MiniParser::parse(Rule::file, &src)?;
    let root = parsed.next().unwrap();
    let rust_code = generate_rust(root);

    fs::write("output.rs", &rust_code)?;
    println!("=== Generated Rust Code (output.rs) ===");
    println!("{}", rust_code);
    Ok(())
}
