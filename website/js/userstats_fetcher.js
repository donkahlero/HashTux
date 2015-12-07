var items = [];
var term = "search_term_year";

//item constructor
function item(name, value){
    this.name = name;
    this.value = value;
}

// Sends a request to fetch userstats by the given term.
function fetch(term) {
    items = [];
    var options = {request_type: "stats", options: []};
    $.ajax({
        url: "/ajax_post.php?search=" + term,
        type: "post",                    
        data: JSON.stringify(options),
                    
        success: function (myString) { 
            parse_to_items(myString);
        }
    });
}
/*
Creates an object of the json and creates objects for every single key value 
pair in the json and then pushes them into the items array.
*/
function parse_to_items(json) {
    var jsonobj = $.parseJSON(json);
    for(var i in jsonobj) {
        var itm = new item(jsonobj[i].key, jsonobj[i].value);
        items.push(itm);
    }
}
