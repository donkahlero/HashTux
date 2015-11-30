var items = [];
var term = "by_search_term";
        
function item(name, value){
    this.name = name;
    this.value = value;
}
/*        
function setSearch(term) {
    this.term = term;
}
*/
function fetch() {
    var options = {request_type: "stats", options: []};
    alert("fetch is starting");
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
        var itm = item(jsonobj[i].key, jsonobj[i].value);
        items.push(itm);
    }
    
        alert(items);
}
    
/**
for (var key in data) {          
       console.log(key + '----' + data[key]);
   }*/