use std::collections::LinkedList;
use super::utils::*;

#[derive(Debug, Clone)]
pub enum Ast
{
	Num(f64),
	String(String),
	Bool(bool),
	Ident(String),
	Vec(Vec<Ast>),
	List(LinkedList<Ast>),
	Call(Box<Ast>, Vec<Ast>),
}

fn _parse(i: &mut usize, tokens: Vec<String>, exit_on_rparen: bool, exit_on_rbrace: bool) -> Vec<Ast>
{
	let mut ast = vec![];
	while *i < tokens.len()
	{
		let tok = tokens.get(*i).unwrap().clone();
		if is_string_numeric(tok.clone())
		{
			ast.push(Ast::Num(tok.parse::<f64>().unwrap()));
		}
		else if tok.clone() == "'"
		{
			*i += 1;
			if *i < tokens.len() && tokens.get(*i).unwrap().clone() == "("
			{
				*i += 1;
				if *i >= tokens.len()
				{
					panic!("unexpected end of input");
				}

				ast.push(Ast::List(LinkedList::from_iter(_parse(i, tokens.clone(), true, false))));
			}
			else
			{
				panic!("Expected '(' after '");
			}
		}
		else if tok.clone() == "("
		{
			*i += 1;
			if *i >= tokens.len()
			{
				panic!("unexpected end of input");
			}

			let content = _parse(i, tokens.clone(), true, false);
			if content.is_empty()
			{
				panic!("expected callable");
			}
			ast.push(Ast::Call(
				Box::new(content.get(0).unwrap().clone()),
				content.get(1..).unwrap().to_vec()
			));
		}
		else if tok.clone() == ")"
		{
			if exit_on_rparen
			{
				return ast;
			}

			panic!("Unexpected character ')'");
		}
		else if tok.clone() == "["
		{
			*i += 1;
			if *i >= tokens.len()
			{
				panic!("unexpected end of input");
			}

			ast.push(Ast::Vec(_parse(i, tokens.clone(), false, true)));
		}
		else if tok.clone() == "]"
		{
			if exit_on_rbrace
			{
				return ast;
			}

			panic!("Unexpected character ']'");
		}
		else if tok.clone().starts_with('"')
		{
			if !tok.clone().ends_with('"')
			{
				panic!("unterminated string");
			}

			ast.push(Ast::String(tok.clone()[1..tok.len() - 1].to_string()));
		}
		else if tok.clone() == "#t"
		{
			ast.push(Ast::Bool(true));
		}
		else if tok.clone() == "#f"
		{
			ast.push(Ast::Bool(false));
		}
		else
		{
			ast.push(Ast::Ident(tok));
		}

		*i += 1;
	}

	if exit_on_rbrace || exit_on_rparen
	{
		panic!("unexpected end of input");
	}

	ast
}

pub fn parse(tokens: Vec<String>) -> Vec<Ast>
{
	_parse(&mut 0, tokens, false, false)
}
