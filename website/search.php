<!DOCTYPE html>

<?php
    $search = $_GET['search'];
?>

<html>
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        <link href="css/hashtux.css" rel="stylesheet">
        
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
                this.userlink = userlink;       // The link to the profile/channel page, depending on the service
                this.frozen = frozen;           // A boolean to check if the content is frozen (frozen will not refresh)
                this.tile = tile;               // Corresponding to the tile ID if the content is being displayed
            }
            
            /**
             * A function that handles the initial request for data from the backend.
             * This request is done with ajax and our internal request type is set to search.
             */
            
            function initialize() {
                
                loading();
                
                $.ajax({
                    url: "/ajax.php?search=" + searchterm + "&request_type=search",
                    type: "get",
                    
                    success: function (myString) {
//                        $('#debug').html("<p>" + myString + "</p>");
                        parse_to_items(myString);   // Parse the JSON to items
                        initDisplayed();            // Run the initDisplayed function
                        initGrid();                 // Initialize the grid
                        stopLoading();
                    }
                });
            }
            
            function reinitialize() {
            
                loading();
            
                displayed = [];
                items = [];
                $('#grid').html('');
                
                options.request_type = "search";
                
                $.ajax({
                    url: "/ajax_post.php?search=" + searchterm,
                    type: "post",
                    data: JSON.stringify(options),
                    
                    success: function (myString) {
//                        $('#debug').html("<p>" + myString + "</p>");
                        parse_to_items(myString);   // Parse the JSON to items
                        initDisplayed();            // Run the initDisplayed function
                        initGrid();                 // Initialize the grid
                        stopLoading();
                    }
                });
                
                options.request_type = "update";
            }
            
            function newSearch() {
                
                hideNoResults();
                
                var newTerm = $('#searchField').val();
                
//                alert("New Search: " + newTerm);
                
                if(newTerm === "")
                {
                    $('#invalidterm').fadeTo(3000, 500).slideUp(500, function() {
                        $('#invalidterm').alert('close');
                    });
                }
                
                else
                {
                    displayed = [];
                    items = [];
                    $('#grid').html('');
                    
                    searchterm = newTerm;
                    
            $('#searchlabel').html("#" + searchterm);
                    reinitialize();
                }
            }
            
            function heartbeat() {
                $.ajax({
                    url: "/ajax.php?search=" + searchterm + "&request_type=heartbeat",
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
//                        alert(JSON.stringify(myString));
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
                
//                var debug = "";

//                alert(json);
                
                if(json === "[]")
                {
                    showNoResults();
                }
                
                else
                {
                    hideNoResults();
                }
                
                var jsonobj = $.parseJSON(json);    // Parse the JSON object
                
                // A for loop to go through all of the objects within the JSON
                // and extract them into item objects.
                
                for(var i in jsonobj) {
                    
                    // Construct an item object.
                    
                    var incItem = new item(
                                jsonobj[i].content_type, jsonobj[i].service,
                                jsonobj[i].resource_link_high, jsonobj[i].text,
                                jsonobj[i].username, jsonobj[i].profile_link, false, "");
                    
//                    debug += jsonobj[i].service;
                                
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
                        
                        if(items.length >= 50)
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
//                
//                alert(debug);
            }
            
          /**
           * A function that runs as soon as the users window loads.
           */
            
            window.onload = function() {
                
                initialize();       // Run the initialize function
                
                refreshTimer = setInterval(refresh, 5000);      // Initialize the refresh function on a timer.
                setInterval(heartbeatFetchHandler, 30000);      // Initialize the heartbeatFetchHandler function on a timer.
                
                hoverListener();    // Run the hoverListener function
            };
            
            // Initialize all bootstrap tooltips on the website
            
            $(function () {
                $('[data-toggle="tooltip"]').tooltip()
            });
            
            // Shows the textfield used to make a new search when you click the search button
            
            function showField() {
                $('#searchField').fadeIn(500);           // Fade in the text field
                $('#searchBtn').hide();             // Hide the search button
                
                $('#searchField').click(function(e) {
                    e.stopPropagation();        // Ignore 
                });
            }
            
            function showMenu() {
                $('#menuBtnTop').fadeOut(200);
                $('#optionsMenu').fadeIn(500);
                $('#topbackdrop').fadeIn(500);
            }
            
            function hideMenu() {
                $('#searchField').hide();
                $('#optionsMenu').fadeOut(500);
                $('#topbackdrop').fadeOut(500);
                $('#searchBtn').show();
                $('#menuBtnTop').fadeIn(200);
            }
            
            function showBottomMenu() {
                $('#menuBtnBottom').fadeOut(200);
                $('#bottombackdrop').fadeIn(500);
                $('#actionsMenu').fadeIn(500);
            }
            
            function hideBottomMenu() {
                $('#menuBtnBottom').fadeIn(200);
                $('#bottombackdrop').fadeOut(500);
                $('#actionsMenu').fadeOut(500);
            }
            
            function loading() {
                $('#progress').show();
            }
            
            function stopLoading() {
                $('#progress').hide();
            }
            
            function showNoResults() {
                $('#no-results').show();
            }
            
            function hideNoResults() {
                $('#no-results').hide();
            }
            
            function runScript(e) {
                if (e.keyCode === 13) {
                    newSearch();
                }
            }
            
            function hoverListener() {
                
                $('body').mouseover(function(e){
                    var x = e.pageX - this.offsetLeft;
                    var y = e.pageY - this.offsetTop;
                    
                    var topLimit = ($(document).height()/3);
                    var bottomLimit = (topLimit * 2);
                    
//                    if(y < topLimit && menuShowing === false)
//                    {
//                        showMenu();
//                        menuShowing = true;
//                    }
                    
                    if(y > topLimit)
                    {
                        hideMenu();
                    }
                    
                    if(y < bottomLimit)
                    {
                        hideBottomMenu();
                    }
                });
            }
	</script>
	          
    </head>
    
    <body style="background-color: #3d3d3d">
 
        <div class="container con-fill">

            <div class="container con-fill header" id="grid">
                <!--<div class="panel" id="debug"></div>-->
            </div>

            <div class="container con-fill-hor">
                
                <div class="topbackdrop" id="topbackdrop"></div>
                
                <button type="submit" class="menubtntop" id="menuBtnTop" onmouseover="showMenu()">
                    <img src="images/menuicon.png" width="50px" height="50px"/>
                </button>
                
                <div class="bottombackdrop" id="bottombackdrop"></div>
                
                <button type="submit" class="menubtnbottom" id="menuBtnBottom" onmouseover="showBottomMenu()">
                    <img src="images/menuicon.png" width="50px" height="50px"/>
                </button>

                <div class="row topbar" id="optionsMenu">
                    <div class="col-sm-8">
                        <ol class="breadcrumb" style="background:none; margin: 0; padding: 0;">
                            <li id="searchlabel" style="font-weight: bold; color: #ebebeb;"><script>document.write("#" + searchterm);</script></li>
                        </ol>
                    </div>
                    <div class="col-sm-4">
                        <button type="submit" class="iconbtn" id="optionsBtn"
                                data-toggle="tooltip" data-placement="bottom" title="Click here to open the options menu"
                                style="float:right;" onclick="showOptions()">
                            <img src="images/options.png" width="30px" height="30px"/>
                        </button>
                        
                        <input type="text" class="searchfield" id="searchField" onkeypress="runScript(event)"
                            style="display: none; float: right; width: 70%; margin-right: 15px; opacity: 0.9;">
                            
                        <button type="submit" class="iconbtn" id="searchBtn"
                                data-toggle="tooltip" data-placement="bottom" title="Click here to enter a new search term"
                                style="float:right; margin-right: 15px;" onmouseover="showField()">
                            <img src="images/search.png" width="30px" height="30px"/>
                        </button>
                        <div class="centered" id="player">
                            <img id="pause" src="http://codropspz.tympanus.netdna-cdn.com/codrops/wp-content/uploads/2010/01/pause1-150x150.png" width="100" height="100" style="display:none;"/>
                            <img id="play" src="http://codropspz.tympanus.netdna-cdn.com/codrops/wp-content/uploads/2010/01/play1-150x150.png" width="100" height="100" style="display:none;"/>
                             
                       </div>
                        
                    </div>
                </div>
                
                <div class="row bottombar" id="actionsMenu">
                    <div class="col-sm-11">
                    </div>
                    <div class="col-sm-1">
                        <button type="submit" class="iconbtn" id="freezeBtn"
                                data-toggle="tooltip" data-placement="top" title="Click here to freeze the screen"
                                style="float: right;" onclick="screenFreeze()">
                            <img src="images/freeze.png" width="40px" height="40px"/>
                        </button>
                    </div>
                </div>
            </div>
                
            <div class="alert-warning gridalert" id="aborted">
                Your changes were aborted!
            </div>
            
            <div class="alert-success gridalert" id="saved">
                Changes saved!
            </div>
            
            <div class="alert-warning gridalert" id="invalidterm">
                You did not enter a hashtag, please try again!
            </div>
            
            <div class="center-container">
                <div class="spinner" id="progress"></div>
                <div class="no-results" id="no-results">NO RESULTS</div>
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
                                    data-toggle="tooltip" data-placement="bottom" title="Small tile size (4x6 grid)"
                                    id="size-sm" onclick="changeSize('size-sm')">SMALL</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Medium tile size (3x4 grid)"
                                    id="size-md" onclick="changeSize('size-md')">MEDIUM</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Large tile size (2x3 grid)"
                                    id="size-lg" onclick="changeSize('size-lg')">LARGE</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Refresh Rate</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Slow update speed (1 tile every 10 seconds)"
                                    id="ref-slow" onclick="changeRefRate('ref-slow')">SLOW</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Medium update speed (1 tile every 5 seconds)"
                                    id="ref-md" onclick="changeRefRate('ref-md')">MEDIUM</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Fast update speed (1 tile every 2 seconds)"
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
                                    data-toggle="tooltip" data-placement="bottom" title="Restrict any images from showing up in your grid"
                                    id="type-img" onclick="changeType('type-img')">Images</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Restrict any videos from showing up in your grid"
                                    id="type-vid" onclick="changeType('type-vid')">Videos</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Restrict any pure text content from showing up in your grid"
                                    id="type-txt" onclick="changeType('type-txt')">Text</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Services</div>
                    <div class="optionexpltext" align="center">Select from which services you would like your content retrieved from.</div>

                    <div class="text-center">
                        <div class="btn-group options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Restrict any content from Twitter to show up in your grid"
                                    id="serv-twitter" onclick="changeService('serv-twitter')">Twitter</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Restrict any content from Instagram to show up in your grid"
                                    id="serv-instagram" onclick="changeService('serv-instagram')">Instagram</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="bottom" title="Restrict any content from YouTube to show up in your grid"
                                    id="serv-youtube" onclick="changeService('serv-youtube')">YouTube</button>
                        </div>
                    </div>

                    <div class="optiontext" align="center">Languages</div>

                    <div class="text-center">
                        <div class="btn-group-vertical options">
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only english content will appear in the grid"
                                    id="en" onclick="changeLanguage('en')">English</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only spanish content will appear in the grid"
                                    id="es" onclick="changeLanguage('es')">Español (Spanish)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only french content will appear in the grid"
                                    id="fr" onclick="changeLanguage('fr')">Français (French)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only german content will appear in the grid"
                                    id="de" onclick="changeLanguage('de')">Deutsch (German)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only swedish content will appear in the grid"
                                    id="sv" onclick="changeLanguage('sv')">Svenska (Swedish)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only bulgarian content will appear in the grid"
                                    id="bg" onclick="changeLanguage('bg')">български език (Bulgarian)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only italian content will appear in the grid"
                                    id="it" onclick="changeLanguage('it')">Italiano (Italian)</button>
                            <button type="button" class="btn btn-default btn-md"
                                    data-toggle="tooltip" data-placement="left" title="Only amharic content will appear in the grid"
                                    id="am" onclick="changeLanguage('am')">አማርኛ (Amharic)</button>
                        </div>
                    </div>

                    <button type="button" class="btn btn-default savebutton"
                            data-toggle="tooltip" data-placement="top" title="Save all options (Grid will be refreshed)"
                            id="save" onclick="saveOptions()">Save & Exit</button>

                </div>

            </div>

        </div>
	    
    </body>
</html>