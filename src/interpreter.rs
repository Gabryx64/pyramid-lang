use std::{
	collections::HashMap,
	rc::Rc,
	fmt::{Debug, Display},
	cell::RefCell
};

use super::parser::*;

#[derive(Debug, Clone)]
pub enum List
{
	Empty,
	Cons(Box<Value>, Box<List>)
}

impl List
{
	pub fn from_slice(vec: &[Value]) -> List
	{
		let mut ret = List::Empty;
		for elem in vec.iter().rev()
		{
			ret = List::Cons(Box::new(elem.clone()), Box::new(ret));
		}
		ret
	}

	pub fn as_vec(&self) -> Vec<Value>
	{
		if let List::Cons(elem, tail) = self
		{
			vec![*elem.clone()]
				.into_iter()
				.chain(tail
					.as_vec()
					.into_iter()
				).collect()
		}
		else
		{
			vec![]
		}
	}
}

impl Display for List
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		write!(f, "'({})",
			self.as_vec()
				.iter()
				.map(|x| format!("{}", x))
				.collect::<Vec<String>>()
				.join(" ")
		)
  }
}

#[derive(Debug, Clone)]
pub enum ArgCnt
{
	Fixed(usize),
	Variadic(usize),
}

type Scope = Rc<RefCell<Vec<HashMap<String, Value>>>>;
type Lambda = Rc<RefCell<dyn FnMut(Vec<Value>) -> Value>>;
type BuiltIn = Rc<RefCell<dyn FnMut(Scope, Vec<Ast>) -> Value>>;

pub enum Value
{
	Num(f64),
	String(String),
	Bool(bool),
	List(List),
	Vec(Vec<Value>),
	Symbol(String),
	Lambda(ArgCnt, Lambda),
	BuiltIn(ArgCnt, BuiltIn),
	Nil,
}

impl Display for Value
{
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match self
		{
			Value::Num(n) => write!(f, "{}", n),
			Value::String(s) => write!(f, "{}", s),
			Value::Bool(b) => write!(f, "{}", b),
			Value::List(l) => write!(f, "{}", l),

			Value::Vec(v) => write!(f, "[{}]",
				v.iter()
					.map(|x| format!("{}", x))
					.collect::<Vec<String>>()
					.join(" ")),

			Value::Symbol(s) => write!(f, "{}", s),
			Value::Lambda(_, _) => write!(f, "<lambda>"),
			Value::BuiltIn(_, _) => write!(f, "<builtin>"),
			Value::Nil => write!(f, "nil"),
		}
	}
}

impl Clone for Value
{
	fn clone(&self) -> Self
	{
		match self
		{
			Value::Num(n) => Value::Num(*n),
			Value::String(s) => Value::String(s.clone()),
			Value::Bool(b) => Value::Bool(*b),
			Value::List(l) => Value::List(l.clone()),
			Value::Vec(v) => Value::Vec(v.clone()),
			Value::Symbol(s) => Value::Symbol(s.clone()),
			Value::Lambda(cnt, f) => Value::Lambda(cnt.clone(), Rc::clone(f)),
			Value::BuiltIn(cnt, f) => Value::BuiltIn(cnt.clone(), Rc::clone(f)),
			Value::Nil => Value::Nil,
		}
	}
}

impl Debug for Value
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
		{
      match self
			{
        Self::Num(arg0) => f.debug_tuple("Num").field(arg0).finish(),
        Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
        Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
        Self::List(arg0) => f.debug_tuple("List").field(arg0).finish(),
        Self::Vec(arg0) => f.debug_tuple("Vec").field(arg0).finish(),
        Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
        Self::Lambda(arg0, _) | Self::BuiltIn(arg0, _) => f.debug_tuple("Lambda").field(arg0).finish(),
        Self::Nil => write!(f, "Nil"),
      }
    }
}

fn _interpret(scopes: Scope, ast: Vec<Ast>, allow_symbol: bool) -> Value
{
	let mut ret = Value::Nil;

	for ast in ast
	{
		match ast
		{
    	Ast::Num(x) => ret = Value::Num(x),
    	Ast::String(x) => ret = Value::String(x),
    	Ast::Bool(x) => ret = Value::Bool(x),
    	Ast::Ident(x) => 
			{
				let mut found = false;
				let scopes = unsafe
				{
					scopes.as_ptr().as_ref().unwrap()
				};

				for scope in scopes
				{
					if let Some(value) = scope.get(&x)
					{
						ret = value.clone();
						found = true;
						break;
					}
				}

				if !found
				{
					if allow_symbol
					{
						ret = Value::Symbol(x);
					}
					else
					{
						panic!("Undefined variable or function {}", x);
					}
				}
			},

    	Ast::Vec(x) => ret = Value::Vec(x
				.iter()
				.map(|x| _interpret(
					Rc::clone(&scopes),
					vec![x.clone()],
					false
				))
				.collect()),

    	Ast::List(x) => ret =
			{
				let mut ret = List::Empty;
				for ast in x.iter().rev()
				{
					ret = List::Cons(Box::new(_interpret(Rc::clone(&scopes), vec![ast.clone()], false)), Box::new(ret));
				}
				Value::List(ret)
			},

    	Ast::Call(func, args) =>
			{
				let func = _interpret(Rc::clone(&scopes), vec![*func], false);

				match func.clone()
				{
					Value::Lambda(cnt, f) =>
					{
						let args: Vec<Value> = args
							.iter()
							.map(|x| _interpret(
								Rc::clone(&scopes),
								vec![x.clone()],
								false
							))
							.collect();

						match cnt
						{
							ArgCnt::Fixed(n) => match args.len()
							{
								l if l == n =>
									ret = unsafe 
									{
										(*f.as_ptr())(args)
									},
									
								l if l < n => ret = Value::Lambda(
									ArgCnt::Fixed(n - l),
									Rc::new(RefCell::new(move |new_args: Vec<Value>| 
									{
										unsafe 
										{
											(*f.as_ptr())(args.clone().into_iter().chain(new_args.into_iter()).collect())
										}
									}))
								),

								_ => panic!("Too many arguments for function {:?}", func),
							},

							ArgCnt::Variadic(n) => match args.len()
							{
								l if l == n => ret = unsafe 
								{
									(*f.as_ptr())(args)
								},

								l => ret = Value::Lambda(
									ArgCnt::Variadic(n - l),
									Rc::new(RefCell::new(move |new_args: Vec<Value>| 
									{
										unsafe 
										{
											(*f.as_ptr())(args.clone().into_iter().chain(new_args.into_iter()).collect())
										}
									}))
								)
							}
						}
					},

					Value::BuiltIn(cnt, f) =>
					{
						match cnt
						{
							ArgCnt::Fixed(n) => match args.len()
							{
								l if l == n => ret = unsafe 
								{
									(*f.as_ptr())(Rc::clone(&scopes), args)
								},

								l if l < n => ret = Value::BuiltIn(
									ArgCnt::Fixed(n - l),
									Rc::new(RefCell::new(move |scopes: Scope, new_args: Vec<Ast>| 
									{
										unsafe 
										{
											(*f.as_ptr())(Rc::clone(&scopes), args.clone().into_iter().chain(new_args.into_iter()).collect())
										}
									}))
								),

								_ => panic!("Too many arguments for function {:?}", func),
							},

							ArgCnt::Variadic(n) => match args.len()
							{
								l if l >= n => ret = unsafe 
								{
									(*f.as_ptr())(Rc::clone(&scopes), args)
								},
								
								l => ret = Value::BuiltIn(
									ArgCnt::Variadic(n - l),
									Rc::new(RefCell::new(move |scopes: Scope, new_args: Vec<Ast>| 
									{
										unsafe 
										{
											(*f.as_ptr())(Rc::clone(&scopes), args.clone().into_iter().chain(new_args.into_iter()).collect())
										}
									}))
								),
							}
						}
					},
					_ => panic!("Cannot call non-function {:?}", func),
				}
			},
		}
	}

	ret
}

pub fn global_scope() -> HashMap<String, Value>
{
	let mut ret = HashMap::new();
	ret.insert("car".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			match &args[0]
			{
				Value::List(l) => match l
				{
					List::Empty => panic!("internal error"),
					List::Cons(car, _) => *car.clone(),
				}
				_ => panic!("cannot get head of non-list"),
			}
		}
	))));

	ret.insert("cdr".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			match &args[0]
			{
				Value::List(l) => match l
				{
					List::Empty => panic!("internal error"),
					List::Cons(_, cdr) => match **cdr
					{
						List::Empty => Value::Nil,
						List::Cons(_, _) => Value::List(*cdr.clone()),
					},
				}
				_ => panic!("cannot get head of non-list"),
			}
		}
	))));

	ret.insert("def".to_string(), Value::BuiltIn(ArgCnt::Fixed(2), Rc::new(RefCell::new(
		|scopes: Scope, args: Vec<Ast>| 
		{
			let id = _interpret(Rc::clone(&scopes), vec![args[0].clone()], true);
			let val = _interpret(Rc::clone(&scopes), vec![args[1].clone()], true);
			match id
			{
				Value::Symbol(s) |
				Value::String(s) =>
				{
					unsafe
					{
						let len = scopes.as_ptr().as_ref().unwrap().len();
						scopes.as_ptr().as_mut().unwrap()[len - 1].insert(s, val)
					}
				},

				_ => panic!("def: expected symbol"),
			};

			Value::Nil
		}
	))));

	ret.insert("lambda".to_string(), Value::BuiltIn(ArgCnt::Variadic(2), Rc::new(RefCell::new(
		|scopes: Scope, args: Vec<Ast>| 
		{
			let mut new_args = Vec::new();
			match args[0].clone()
			{
				Ast::List(l) => for node in l
				{
					match _interpret(Rc::clone(&scopes), vec![node], true)
					{
						Value::Symbol(s) |
						Value::String(s) => new_args.push(s),
						_ => panic!("lambda: expected symbol"),
					}
				},

				Ast::Vec(v) => for node in v
				{
					match _interpret(Rc::clone(&scopes), vec![node], true)
					{
						Value::Symbol(s) |
						Value::String(s) => new_args.push(s),
						_ => panic!("lambda: expected symbol"),
					}
				},

				_ => panic!("lambda: expected list"),
			};

			Value::Lambda(ArgCnt::Fixed(new_args.len()), Rc::new(RefCell::new(
				move |largs: Vec<Value>|
				{
					let mut new_scope = HashMap::new();
					for (i, arg) in new_args.clone().into_iter().enumerate()
					{
						new_scope.insert(arg, largs[i].clone());
					}

					unsafe
					{
						scopes.as_ptr().as_mut().unwrap().push(new_scope);
						let ret = _interpret(Rc::clone(&scopes), vec![args.clone()[args.len() - 1].clone()], true);
						scopes.as_ptr().as_mut().unwrap().pop();
						ret
					}
				}
			)))
		}
	))));

	ret.insert("list->vec".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			match &args[0]
			{
				Value::List(l) => Value::Vec(l.as_vec()),
				_ => panic!("cannot convert non-list to vector using list->vec"),
			}
		}
	))));

	ret.insert("vec->list".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			match &args[0]
			{
				Value::Vec(v) => Value::List(List::from_slice(v)),
				_ => panic!("cannot convert non-vector to list using vec->list"),
			}
		}
	))));

	ret.insert("vec=>".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			let v = match &args[0]
			{
				Value::Vec(v) => v,
				_ => panic!("cannot get element of non-vector using vec=>"),
			};

			let idx = match args[1]
			{
				Value::Num(v) => if v != v.round() || v < 0.0
				{
					panic!("index needs to be a non-negative integer")
				}
				else
				{
					v as usize
				},
				_ => panic!("index needs to be a non-negative integer"),
			};

			v[idx].clone()
		}
	))));

	ret.insert("=>vec".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			let mut v = match &args[0]
			{
				Value::Vec(v) => v.clone(),
				_ => panic!("cannot set element of non-vector using =>vec"),
			};

			let idx = match args[1]
			{
				Value::Num(v) => if v != v.round() || v < 0.0
				{
					panic!("index needs to be a non-negative integer")
				}
				else
				{
					v as usize
				},
				_ => panic!("index needs to be a non-negative integer"),
			};

			v[idx] = args[2].clone();
			Value::Vec(v)
		}
	))));

	ret.insert("->string".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| Value::String(format!("{}", args[0]))
	))));

	ret.insert("print".to_string(), Value::Lambda(ArgCnt::Variadic(0), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			for arg in args
			{
				print!("{}", arg);
			}
			println!();
			Value::Nil
		}
	))));

	ret.insert("+".to_string(), Value::Lambda(ArgCnt::Variadic(2), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			let mut sum = 0.0;
			for arg in args
			{
				match arg
				{
					Value::Num(x) => sum += x,
					_ => panic!("Cannot add non-number"),
				}
			}

			Value::Num(sum)
		}
	))));

	ret.insert("-".to_string(), Value::Lambda(ArgCnt::Variadic(2), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			let mut sum = 0.0;
			let mut first = true;
			for arg in args
			{
				match arg
				{
					Value::Num(x) => if first
					{
						sum = x;
						first = false;
					}
					else
					{
						sum -= x
					},

					_ => panic!("Cannot subtract non-number"),
				}
			}

			Value::Num(sum)
		}
	))));

	ret.insert("*".to_string(), Value::Lambda(ArgCnt::Variadic(2), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			let mut sum = 1.0;
			for arg in args
			{
				match arg
				{
					Value::Num(x) => sum *= x,
					_ => panic!("Cannot multiply non-number"),
				}
			}

			Value::Num(sum)
		}
	))));

	ret.insert("/".to_string(), Value::Lambda(ArgCnt::Variadic(2), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			let mut sum = 1.0;
			let mut first = true;
			for arg in args
			{
				match arg
				{
					Value::Num(x) => if first
					{
						sum = x;
						first = false;
					}
					else
					{
						sum -= x
					},
					_ => panic!("Cannot divide non-number"),
				}
			}

			Value::Num(sum)
		}
	))));

	ret.insert("exit".to_string(), Value::Lambda(ArgCnt::Fixed(1), Rc::new(RefCell::new(
		|args: Vec<Value>| 
		{
			match args[0]
			{
				Value::Num(x) => if x != x.round()
				{
					panic!("Cannot exit with a non-integer")
				}
				else
				{
					std::process::exit(x as i32);
				},

				_ => panic!("Cannot exit with a non-integer"),
			}
		}
	))));

	ret
}

pub fn interpret(global_scope: &mut HashMap<String, Value>, ast: Vec<Ast>) -> Value
{
	let scopes = Rc::new(RefCell::new(vec![global_scope.clone()]));

	let ret = _interpret(Rc::clone(&scopes), ast, true);
	unsafe
	{
		*global_scope = scopes.as_ptr().as_ref().unwrap()[0].clone();
	}
	ret
}
