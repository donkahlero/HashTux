
function strip_illegal_characters(str) {
	return str.replace("/^[A-Za-z0-9_- \u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]/gi", "");
}
