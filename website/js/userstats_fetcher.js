var items = [];
var term = "search_term_year";
        
function item(name, value){
    this.name = name;
    this.value = value;
}
/*        
function setSearch(term) {
    this.term = term;
}
*/
function fetch(term) {
    items = [];
    var options = {request_type: "stats", options: []};
    $.ajax({
        url: "/ajax_post.php?search=" + term,
        type: "post",                    
        data: JSON.stringify(options),
                    
        success: function (myString) { 
            alert(myString);
            parse_to_items(myString);
        }
    });
}
        
function parse_to_items(json) {
    var jsonobj = $.parseJSON(json);
    for(var i in jsonobj) {
        var itm = new item(jsonobj[i].key, jsonobj[i].value);
        items.push(itm);
    }
}
    
/**
for (var key in data) {          
       console.log(key + '----' + data[key]);
   }*/