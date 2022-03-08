pub fn is_string_numeric(str: String) -> bool
{
	let mut found_dot = false;
	for c in str.chars()
	{
		if !c.is_numeric() && c != '.'
		{
			return false;
		}

		if c == '.'
		{
			if found_dot
			{
				return false;
			}

			found_dot = true;
		}
	}

	true
}
