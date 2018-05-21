use std::process::Command;

fn get_source(filename: &str) -> String {
	let output = Command::new("java")
		.arg("-cp")
		.arg("/home/andrew/Documents/2184-csc431/given_parser/antlr-4.7.1-complete.jar:/home/andrew/Documents/2184-csc431/given_parser/")
		.arg("MiniCompiler")
		.arg("/home/andrew/Documents/2184-csc431/given_parser/examples/t1.co")
		.output()
		.expect("Failed to parse program");

	return String::from(String::from_utf8_lossy(&output.stdout));
}

fn main() {
	println!("{}", get_source("oops"));
}
