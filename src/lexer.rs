pub fn tokenize<T>(src: T) -> Vec<String> where T: Into<String>
{
	let reg_str = r#"[\s,]*(~@|[\[\]()']|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#;
	let reg = regex::Regex::new(reg_str).unwrap();

	reg.captures_iter(&src.into())
		.map(|cap| cap[1].to_string())
		.collect()
}