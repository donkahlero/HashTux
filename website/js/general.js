/**
 * Removes illegal characters (intended for cleaning the search term entered by the user)
 * other than normal "word" caracters (including western such as åäö, ü), _, - and numbers.
 * @param str
 * @returns	The string, now containing only reasonable characters
 */
function strip_illegal_characters(str) {
	return str.replace("/^[A-Za-z0-9_- \u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]/gi", "");
}


/**
 * Functions for showing and hiding the commercial use box
 */
function showCommercialUseInfo() {
    $('#commercial_use').fadeIn(500);
    $('#commercial_panel').click(function(e) {
        e.stopPropagation();
    });
}

function hideCommercialUseInfo() {
    $('#commercial_use').fadeOut(500);
}