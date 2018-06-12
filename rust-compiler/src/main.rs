extern crate mini;

use std::process::Command;
use mini::mini_ast::generate_ast;

fn get_source(filename: &str) -> String {
	let output = Command::new("java")
		.arg("-cp")
		.arg("/home/andrew/Documents/2184-csc431/given_parser/antlr-4.7.1-complete.jar:/home/andrew/Documents/2184-csc431/given_parser/")
		.arg("MiniCompiler")
		.arg(filename)
		.output()
		.expect("Failed to parse program");

	return String::from(String::from_utf8_lossy(&output.stdout));
}

fn main() {
    let ast = generate_ast(
        &get_source("/home/andrew/Documents/2184-csc431/given_parser/examples/t2.co"));

	println!("{}", ast);
}
