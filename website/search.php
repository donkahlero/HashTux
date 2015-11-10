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
            
            var items = [];
            var displayed = [];
            
            var gridWidth = 4;
            var gridHeight = 3;
            var totalItems = gridWidth * gridHeight;
            
            function item(type, url, text, username, timestamp, frozen, tile) {
                this.type = type;
                this.url = url;
                this.text = text;
                this.username = username;
                this.timestamp = timestamp;
                this.frozen = frozen;
                this.tile = tile;
            }
            
            function initialize() {
                myjson = <?php echo json_encode($output);?>;
                parse_to_items(myjson);
            }
            
            function fetch(){
                
                
                
                // NB RIGHT NOW THE SEARCH TERM IS "HARDCODED" BY PHP
                $.ajax({
                    url: "/ajax.php?search=<?php echo $search; ?>",
                    type: "get",
                    success: function (myString) {
                        parse_to_items(myString);
                    }
                });
            }

            function parse_to_items(json) {
                
                var jsonobj = $.parseJSON(json);
                
                for(var i in jsonobj) {
                    
                    var incItem = new item(jsonobj[i].content_type, jsonobj[i].resource_link_high, jsonobj[i].text, jsonobj[i].username, json[i].timestamp, false, "");
                    var ignore = false;
                    
                    for(j = 0; j < displayed.length; j++)
                    {
                        if(incItem.url === displayed[j].url || incItem.text === displayed[j].text)
                        {
                            ignore = true;
                            break;
                        }
                    }
                    
                    for(k = 0; k < items.length; k++)
                    {
                        if(incItem.url === items[k].url || incItem.text === items[k].text)
                        {
                            ignore = true;
                            break;
                        }
                    }
                    
                    if(ignore === false)
                    {
                        items.push(incItem);
                    }
                }
                
                alert(items.length);
            }
            
            window.onload = function() {
                
                initialize();
                
                var grid = $('#grid');
                var count = 0;
                
                for(k = 0; k < totalItems; k++) {
                    
                    if(items.length < 1)
                        break;
                    
                    var index = Math.floor((Math.random() * items.length));

                    displayed.push(items[index]);
                    items.splice(index, 1);
                }
                
                for(i = 0; i < gridHeight; i++) {
                    
                    var cols = "";
                    
                    for(j = 0; j < gridWidth; j++) {
                        
                        if(count >= displayed.length) {
                            
                            cols = cols + 
                                "<div class='col-xs-" +
                                (12/gridWidth) + 
                                " col-fill imageitem' style='background-image:url();' id='tile" +
                                count +
                                "'></div>";
                        
                            count++;
                        }
                        else if(displayed[count].type === "image") {
                            
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
                        else if(displayed[count].type === "text")
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
                    
                    grid.html(grid.html() +
                        "<div class='row-grid-" +
                        (12/gridHeight) + "' id='row" +
                        i + "'>" + cols + "</div>");
                }
                
            };
            
            setInterval(refresh, 60000);
            
            function refresh() {
                            
                fetch();
                
                if(items.length > 0)
                {
                
                    var newIndex = Math.floor((Math.random() * items.length));
                    var newItem = items[newIndex];
                    var randTileNum = Math.floor((Math.random() * totalItems));
                    var randTile = "#tile" + randTileNum;

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

                        for(i = 0; i < displayed.length; i++)
                        {
                            if(displayed[i].tile === "tile" + randTileNum)
                            {
                                displayed[i].tile === "lawl";
                                items.push(displayed[i]);
                                displayed.splice(i, 1);

                                break;
                            }
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

