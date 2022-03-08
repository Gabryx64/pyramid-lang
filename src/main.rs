use interpreter::Value;
use rustyline::error::ReadlineError;
use rustyline::Editor;

mod utils;
mod lexer;
mod parser;
mod interpreter;

fn main()
{
	let mut global_scope = interpreter::global_scope();
	let mut rl = Editor::<()>::new();

	loop
	{
		let readline = rl.readline("Δ> ");
		match readline
		{
			Ok(line) =>
			{
				rl.add_history_entry(line.as_str());
				let tokens = lexer::tokenize(line.as_str());
				let ast = parser::parse(tokens);
				let result = interpreter::interpret(&mut global_scope, ast);
				match result
				{
					Value::Nil => (),
					_          => println!("{}", result),
				}
			},
			Err(ReadlineError::Interrupted) => break,
			Err(ReadlineError::Eof) => break,
			Err(err) =>
			{
				println!("Error: {:?}", err);
				break;
			},
		}
	}
}
