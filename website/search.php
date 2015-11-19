<?php
    require_once("curl_request.php");
    
    $search = $_GET['search'];
    
    $output = curl_request($search);
?>

<!DOCTYPE html> 
<html>
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        <script type="text/javascript">
            
            var items = [];         // An array to store all items fetched
            var displayed = [];     // An array to temporarily store the currently displayed items
            
            var gridWidth = 4;      // The width of the grid (in num of tiles)
            var gridHeight = 3;     // The Height of the grid (in num of tiles)
            var totalItems = gridWidth * gridHeight;    // total number of tiles

            
            // A constructor that given a certain number of arguments creates item
            // object which in this case is a representation of the JSON objects
            // retreived from the backend.
            
            function item(type, service, url, text, username, frozen, tile) {
                this.type = type;               // The content type
                this.service = service;         // The service the content is from
                this.url = url;                 // URL (img/video)
                this.text = text;               // Text content (tweet)
                this.username = username;       // The username of content provider
                this.frozen = frozen;           // A boolean to check if the content is frozen (frozen will not refresh)
                this.tile = tile;               // Corresponding to the tile ID if the content is being displayed
            }
            
            // A function for the initial fetch when a search is made. In this
            // case the JSON is already fetched from the http server using 'curl'
            // (see top of document)
            
            function initialize() {
                myjson = <?php echo json_encode($output);?>;
                parse_to_items(myjson);
            }
            
            // A function for any future searches. Uses ajax.php to fetch the
            // JSON object from the http server.
            
            function fetch(){
                
                // NB RIGHT NOW THE SEARCH TERM IS "HARDCOD ED" BY PHP
                $.ajax({
                    url: "/ajax.php?search=<?php echo $search; ?>",
                    type: "get",
                    success: function (myString) {
                        parse_to_items(myString);
                    }
                });
            }
            
            // A function to extract all the information from the JSON object and
            // convert it to an amount of item objects that we store in the items
            // array.

            function parse_to_items(json) {
                
                var jsonobj = $.parseJSON(json);    // Parse the JSON object
                
                // A for loop to go through all of the objects within the JSON
                // and extract them into item objects.
                
                for(var i in jsonobj) {
                    
                    // Construct an item object.
                    
                    var incItem = new item(
                                jsonobj[i].content_type, jsonobj[i].service,
                                jsonobj[i].resource_link_high, jsonobj[i].text,
                                jsonobj[i].username, false, "");
                                
                    var ignore = false;     // A boolean to keep track of whether to insert the item or not
                    
                    // Goes through the displayed array to check whether any of
                    // the items in it is the same as the incoming item. If so, disregard
                    // the incoming item.
                    
                    for(j = 0; j < displayed.length; j++)
                    {
                        if(incItem.url === displayed[j].url || incItem.text === displayed[j].text)
                        {
                            ignore = true;      // Set ignore to true
                            break;              // Break the loop and continue with the next item
                        }
                    }
                    
                    // Goes through the items array and checks whether any of the
                    // items are the same as the incoming one. If so, disregard the item.
                    
                    for(k = 0; k < items.length; k++)
                    {
                        if(incItem.url === items[k].url || incItem.text === items[k].text)
                        {
                            ignore = true;      // Set ignore to true
                            break;              // Break the loop and continue with the next item
                        }
                    }
                    
                    // As long as ignore is false, do the following
                    
                    if(ignore === false)
                    {
                        
                        // If the length of the item array reaches 100 elements
                        // push an old item out before inserting a new one
                        
                        if(items.length >= 100)
                        {
                            items.splice(0, 1);     // Remove the head of items
                            items.push(incItem);    // Add the new item to the end of items
                        }
                        
                        else
                        {
                            items.push(incItem);    // Add the new item to the end of items
                        }
                    }
                }
                
                var debug = "";
                
                for(k = 0; k < items.length; k++)
                {
                    debug += items[k].service;
                }
                
                alert(debug);
            }
            
            // A function that runs as soon as the users window loads
            
            window.onload = function() {
                
                initialize();       // Run the initialize function
                initDisplayed();    // Run the initDisplayed function
                initGrid();         // Initialize the grid
                
                setInterval(refresh, 5000);
                setInterval(fetch, 60000);
 
            };
            
            function initGrid() 
            {
                var grid = $('#grid');
                var count = 0;
                
                for(i = 0; i < gridHeight; i++) {
                    
                    var cols = "";
                    
                    for(j = 0; j < gridWidth; j++) {
                        
                        if(count >= displayed.length) 
                        {
                            
                            cols = cols + 
                                "<div class='col-xs-" +
                                (12/gridWidth) + 
                                " col-fill imageitem' style='background-image:url();' id='tile" +
                                count +
                                "'></div>";
                        
                            count++;
                        }
                        
                        else if(displayed[count].service === "instagram") 
                        {
                            
                            if(displayed[count].type === "image")
                            {
                            
                                cols = cols + 
                                    "<div class='col-xs-" +
                                    (12/gridWidth) + 
                                    " col-fill imageitem' style='background-image:url(" +
                                    displayed[count].url +
                                    ");' id='tile" +
                                    count + "'>" +
                                    "<p class='usernameimage'>@" +
                                    displayed[count].username + "</p>" +
                                    "</div>";

                                displayed[count].tile = "tile" + count;
                                count++;
                            
                            }
                            
                            else if(displayed[count].type === "video")
                            {
                                cols = cols +
                                "<div class='col-xs-" + 
                                (12/gridWidth) + 
                                " col-fill imageitem' style='background-image:url('');' id='tile"+
                                count + "'>" +
                                "<p class='twittertext'>[INSERT VIDEO HERE]</p>" +
                                "</div>";
                        
                                displayed[count].tile = "tile" + count;
                                count++;
                            }
                            
                        }
                        
                        else if(displayed[count].service === "twitter")
                        {
                            
                            if(displayed[count].type === "text")
                            { 
                                cols = cols +
                                    "<div class='col-xs-" +
                                    (12/gridWidth) + 
                                    " col-fill twitteritem' id='tile" +
                                    count + "'>" +
                                    "<div class='twittertext'><p>" +
                                    displayed[count].text +
                                    "</p><p class='usernametweet'>@" +
                                    displayed[count].username +
                                    "</p></div>" + 
                                    "</div>";

                                displayed[count].tile = "tile" + count;
                                count++;
                            }
                            
                            else if(displayed[count].type === "image")
                            {
                                cols = cols + 
                                    "<div class='col-xs-" +
                                    (12/gridWidth) + 
                                    " col-fill twitteritem' style='background-image:url(" +
                                    displayed[count].url +
                                    ");' id='tile" +
                                    count + "'>" +
                                    "<div class='twittertext'><p>" +
                                    displayed[count].text +
                                    "</p><p class='usernametweet'>@" +
                                    displayed[count].username +
                                    "</p></div>" + 
                                    "</div>";
                            }
                        }
                    }
                    
                    grid.html(grid.html() +
                        "<div class='row-grid-" +
                        (12/gridHeight) + "' id='row" +
                        i + "'>" + cols + "</div>");
                }
            }
            
            function initDisplayed()
            {                
                if(displayed.length > 0)
                {
                    displayed.length = 0;
                }
                
                for(k = 0; k < totalItems; k++) {
                    
                    if(items.length < 1)
                        break;
                    
                    var index = Math.floor((Math.random() * items.length));

                    displayed.push(items[index]);
                    items.splice(index, 1);
                }
            }
            
            function refresh() 
            {
                
                if(items.length > 0)
                {
                
                    var newIndex = Math.floor((Math.random() * items.length));
                    var newItem = items[newIndex];
                    var randTileNum = Math.floor((Math.random() * totalItems));
                    var randTile = "#tile" + randTileNum;
                    var currentItem;
                    var currentIndex;

                        for(i = 0; i < displayed.length; i++)
                        {
                            if(displayed[i].tile === "tile" + randTileNum)
                            {
                                currentItem = displayed[i];
                                currentIndex = i;
                            }
                        }
                        
                        if(currentItem.frozen === true)
                        {
                            refresh();
                        }

                            if(newItem.type === "image") 
                            {
                                $(randTile).animate({height: "0", opacity: "0"}, 0);

                                $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill imageitem');
                                $(randTile).css('background-image', 'url(' + newItem.url + ')');
                                $(randTile).html("<p class='usernameimage'>@" + newItem.username + "</p>");

                                $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                            }
                            else if(newItem.type === "text")
                            {
                                $(randTile).animate({height: "0", opacity: "0"}, 0);

                                $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill twitteritem');
                                $(randTile).css('background-image', '');
                                $(randTile).html(
                                        "<div class='twittertext'><p>" +
                                        newItem.text +
                                        "</p><p class='usernametweet'>@" +
                                        newItem.username +
                                        "</p></div>");

                                $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                            }
                            else if(newItem.type === "video")
                            {
                                $(randTile).animate({height: "0", opacity: "0"}, 0);

                                $(randTile).attr('class', 'col-xs-' + (12/gridWidth) + ' col-fill twitteritem');
                                $(randTile).css('background-image', '');
                                $(randTile).html(
                                        "<div class='twittertext'><p>" +
                                        "[INSERT VIDEO HERE]" +
                                        "</p><p class='usernametweet'>@" +
                                        newItem.username +
                                        "</p></div>");

                                $(randTile).animate({height: "100%", opacity: "1"}, 1000);
                            }

                            if(currentItem !== null)
                            {
                                currentItem.tile = "";
                                items.push(currentItem);
                                displayed.splice(currentIndex, 1);
                            }

                            newItem.tile = "tile" + randTileNum;
                            displayed.push(newItem);
                            items.splice(newIndex, 1);
                    }
            }
            
            function showField() {
                $('#sField').fadeIn(500);
                $('#searchBtn').hide();
                
                $('#sField').click(function() {
                    event.stopPropagation();
                });
                
                event.stopPropagation();
            }
            
            function showMenu() {
                $('#optionsMenu').fadeIn(500);
            }
            
            function hideMenu() {
                $('#sField').hide();
                $('#optionsMenu').fadeOut(500);
                $('#searchBtn').show();
            }
            
            function hideSearchField() {
                $('#sField').hide();
                $('#searchBtn').fadeIn(500);
            }
            
            function showOptions() {
                $('#options').fadeIn(500);
            }
            
            function hideOptions() {
                $('#options').fadeOut(500);
            }
	</script>
	          
    </head>
    
    <body style="background-color: #3d3d3d">
 
        <div class="container con-fill">

                <div class="container con-fill header" id="grid">
                </div>

                <div class="container con-fill-hor">

                    <div class="tophoverarea" onmouseover="showMenu()" onclick="hideSearchField()"></div>
                    <div class="midhoverarea" onmouseover="hideMenu()"></div>
                    <div class="bothoverarea" onmouseover=""></div>

                    <div class="row topbar" id="optionsMenu">
                        <div class="col-md-8">
                            <ol class="breadcrumb" style="background:none; margin: 0; padding: 0;">
                                <li style="font-weight: bold; color: #ebebeb;">#<?php echo $search; ?></li>
                            </ol>
                        </div>
                        <div class="col-md-4">
                            <button type="button" class="btn btn-default btn-md" id="optionsBtn"
                                    style="float:right;" onclick="showOptions()">
                                O
                            </button>
                            <div class="input-group" style="display: none; float:right; width:inherit; margin-right: 15px;" id="sField">
                                <span class="input-group-addon">#</span>
                                <input type="text" class="form-control" name="sField">
                            </div>
                            <button type="button" class="btn btn-default btn-md" id="searchBtn"
                                    style="float:right; margin-right: 15px;" onclick="showField()">
                                S
                            </button>
                        </div>
                    </div>
                </div>
            
                <div class="container con-fill header" id="options" onclick="hideOptions()" 
                     style="background-color: rgba(0, 0, 0, 0.5); display: none;" >
                </div>

            </div>
	    
    </body>
</html>

