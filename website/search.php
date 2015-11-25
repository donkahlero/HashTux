<!DOCTYPE html>

<?php
    require_once("request.php");
    
    $search = $_GET['search'];
    $options = build_request_options("search");
    $output = request($search, $options); 
?>

<html>
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        
        <script src="js/grid.js"></script>
        <script src="js/refresh.js"></script>
        <script src="js/freeze.js"></script>
        <script src="js/options.js"></script>
        
        
        <script type="text/javascript">
            
            var searchterm = "<?php echo $search; ?>";
            var options = {request_type: "update"};
            
            var heartbeats = true;
            
            var refreshTimer;
            
            var screenFrozen = false;
            
            var items = [];         // An array to store all items fetched
            var displayed = [];     // An array to temporarily store the currently displayed items
            
            var gridWidth = 4;      // The width of the grid (in num of tiles)
            var gridHeight = 3;     // The Height of the grid (in num of tiles)
            var totalItems = gridWidth * gridHeight;    // total number of tiles

            
            // A constructor that given a certain number of arguments creates item
            // object which in this case is a representation of the JSON objects
            // retreived from the backend.
            
            function item(type, service, url, text, username, userlink, frozen, tile) {
                this.type = type;               // The content type
                this.service = service;         // The service the content is from
                this.url = url;                 // URL (img/video)
                this.text = text;               // Text content (tweet)
                this.username = username;       // The username of content provider
                this.userlink = userlink;
                this.frozen = frozen;           // A boolean to check if the content is frozen (frozen will not refresh)
                this.tile = tile;               // Corresponding to the tile ID if the content is being displayed
            }
            
            // A function for the initial fetch when a search is made. In this
            // case the JSON is already fetched from the http server using 'curl'
            // (see top of document)
            
            function initialize() {
                myjson = <?php echo json_encode($output); ?>;
                parse_to_items(myjson);
            }
            
            function reinitialize() {
                displayed = [];
                items = [];
                $('#grid').html('');
                
                initialize();
                initDisplayed();
                initGrid();
            }
            
            function heartbeat(){
                $.ajax({
                    url: "/ajax.php?search=" + searchterm + "&request_type=heartbeat",
                    type: "get",
                    success: function (myString) {
                        parse_to_items(myString);
                    }
                });
            }
            
            // A function for any future searches. Uses ajax.php to fetch the
            // JSON object from the http server.
            
            function fetch() {
                $.ajax({
                    url: "/ajax_post.php?search=" + searchterm,
                    type: "post",                    
                    data: JSON.stringify(options),
                    
                    success: function (myString) {
                        alert(JSON.stringify(myString));
                        parse_to_items(myString);
                    }
                });
            }
            
            function heartbeatFetchHandler() {
                
                if(heartbeats === true)
                {
                    heartbeat();
                    heartbeats = false;
                }
                
                else
                {
                    fetch();
                    heartbeats = true;
                }
            }
            
            // A function to extract all the information from the JSON object and
            // convert it to an amount of item objects that we store in the items
            // array.

            function parse_to_items(json) {
                
                var debug = "";
                
                var jsonobj = $.parseJSON(json);    // Parse the JSON object
                
                // A for loop to go through all of the objects within the JSON
                // and extract them into item objects.
                
                for(var i in jsonobj) {
                    
                    // Construct an item object.
                    
                    var incItem = new item(
                                jsonobj[i].content_type, jsonobj[i].service,
                                jsonobj[i].resource_link_high, jsonobj[i].text,
                                jsonobj[i].username, jsonobj[i].profile_link, false, "");
                    
                    if(incItem.service === "youtube")
                    {
                        debug += "1" + jsonobj[i].content_type + "\n" +
                                    "2" + jsonobj[i].service + "\n" +
                                    "3" + jsonobj[i].resource_link_high + "\n" +
                                    "5" + jsonobj[i].username + "\n" +
                                    "6" + jsonobj[i].profile_link;
                    }
                    
                    debug += jsonobj[i].service
                                
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
                        
                        // If the length of the item array reaches 50 elements
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
                
//                debug += "\n\n";
//                
//                for(k = 0; k < items.length; k++)
//                {
//                    debug += items[k].service + " ";
//                }
                
                alert(debug);
            }
            
            // A function that runs as soon as the users window loads
            
            window.onload = function() {
                
                initialize();       // Run the initialize function
                initDisplayed();    // Run the initDisplayed function
                initGrid();         // Initialize the grid
                
                refreshTimer = setInterval(refresh, 5000);
                setInterval(heartbeatFetchHandler, 30000);
 
            };
            
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
            
	</script>
	          
    </head>
    
    <body style="background-color: #3d3d3d">
 
        <div class="container con-fill">

            <div class="container con-fill header" id="grid">
            </div>

            <div class="container con-fill-hor">

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
                        <div class="centered" id="player">
                            <img id="pause" src="http://codropspz.tympanus.netdna-cdn.com/codrops/wp-content/uploads/2010/01/pause1-150x150.png" width="100" height="100" style="display:none;"/>
                            <img id="play" src="http://codropspz.tympanus.netdna-cdn.com/codrops/wp-content/uploads/2010/01/play1-150x150.png" width="100" height="100" style="display:none;"/>
                             
                       </div>
                        
                    </div>
                </div>
            </div>
                
            <div class="alert-warning gridalert" id="aborted">
                Your changes were aborted!
            </div>
            
            <div class="alert-success gridalert" id="saved">
                Changes saved!
            </div>

            <div class="container con-fill header" id="options" onclick="hideOptions()"
                     style="background-color: rgba(0, 0, 0, 0.5); display: none;" >

                <div class="panel optionspanel" style="margin: auto;" id="optionsPanel">

                    <hr />

                    <h4 align="center">GRID OPTIONS</h4>

                    <hr />

                    <div class="optiontext" align="center">Size</div> 

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    id="size-sm" onclick="changeSize('size-sm')">SMALL</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="size-md" onclick="changeSize('size-md')">MEDIUM</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="size-lg" onclick="changeSize('size-lg')">LARGE</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Refresh Rate</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md" 
                                    id="ref-slow" onclick="changeRefRate('ref-slow')">SLOW</button>
                            <button type="button" class="btn btn-default btn-md" 
                                    id="ref-md" onclick="changeRefRate('ref-md')">MEDIUM</button>
                            <button type="button" class="btn btn-default btn-md" 
                                    id="ref-fast" onclick="changeRefRate('ref-fast')">FAST</button>
                        </div>
                    </div>

                    <hr />

                    <h4 align="center">CONTENT OPTIONS</h4>

                    <hr />

                    <div class="optiontext" align="center">Media Type</div>
                    <div class="optionexpltext" align="center">Select the type of media you would like displayed in the grid.</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    id="type-img" onclick="changeType('type-img')">Images</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="type-vid" onclick="changeType('type-vid')">Videos</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="type-txt" onclick="changeType('type-txt')">Text</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Services</div>
                    <div class="optionexpltext" align="center">Select from which services you would like your content retrieved from.</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    id="serv-twitter" onclick="changeService('serv-twitter')">Twitter</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="serv-instagram" onclick="changeService('serv-instagram')">Instagram</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="serv-youtube" onclick="changeService('serv-youtube')">YouTube</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Languages</div>

                    <div class="text-center">
                        <div class="btn-group-vertical options">
                            <button type="button" class="btn btn-default btn-md"
                                    id="en" onclick="changeLanguage('en')">English</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="es" onclick="changeLanguage('es')">Español (Spanish)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="fr" onclick="changeLanguage('fr')">Français (French)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="de" onclick="changeLanguage('de')">Deutsch (German)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="sv" onclick="changeLanguage('sv')">Svenska (Swedish)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="bg" onclick="changeLanguage('bg')">български език (Bulgarian)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="it" onclick="changeLanguage('it')">Italiano (Italian)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    id="am" onclick="changeLanguage('am')">አማርኛ (Amharic)</button>
                        </div>
                    </div>

                    <button type="button" class="btn btn-default savebutton" id="save" onclick="saveOptions()">Save & Exit</button>

                </div>

            </div>

        </div>
	    
    </body>
</html>