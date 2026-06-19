use pest_derive::Parser;

#[derive(Parser, Debug)]
#[grammar = "../mini_rust.pest"]
pub struct MiniParser;
