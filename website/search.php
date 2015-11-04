<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
        
        require_once("curl_request.php");
        $search = $_GET['search'];

        $output = curl_request($search);

        $output = str_replace("\n", "<br />", $output);
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
            
            function item(type, url, text, username, frozen, displayed) {
                this.type = type;
                this.url = url;
                this.text = text;
                this.username = username;
                this.frozen = frozen;
                this.displayed = displayed;
            }
            
            function fetch(){
                myjson = <?php echo json_encode($output);?>;
                parse_to_items(myjson);
            }

            function parse_to_items(json) {
                
                var jsonobj = $.parseJSON(json);
                
                var test = "";
                
                for(var i in jsonobj) {
                    items.push(new item(jsonobj[i].content_type, jsonobj[i].resource_link_low, jsonobj[i].text, jsonobj[i].username, false, false));
                }
            }
            
            window.onload = function() {
                
                fetch();
    
                var $grid = document.getElementById('grid');
                var $row = "<div class='row-grid-6' id='row";
                var $rowmid = "'>";
                var $rowend = "</div>";
                var $colstart = "<div class='col-xs-4 col-fill imageitem' style='background-image:url(";
                var $colid = ");' id='col";
                var $colidend = "'>";
                var $colend = "</div>";
                var $count = 0;
                
                var $tweetcolstart = "<div class='col-xs-4 col-fill twitteritem' id='col";
                var $tweetcolmid = "'>";
                var $tweetcolend = "</div>";
                
                for(i = 0; i < 2; i++) {
                    
                    var $cols = "";
                    
                    for(j = 0; j < 3; j++) {
                        
                        if($count >= items.length) {
                            $cols = $cols + $colstart + $colmid + $count + $colend;
                            $count++;
                        }
                        
                        else if(items[$count].type === "image") {                        
                            $cols = $cols + $colstart + items[$count].url + $colid + $count + $colidend + "<p class='usernameimage'>@" + items[$count].username + "</p>" + $colend;
                            items[$count].displayed = true;
                            $count++;
                        }
                        else if(items[$count].type === "text")
                        {
                            $cols = $cols + $tweetcolstart + $count + $tweetcolmid +
                                "<div class='twittertext'><p>" +
                                items[$count].content +
                                "</p><p class='usernametweet'>@" +
                                images[$count].username +
                                "</p></div>" + 
                                $tweetcolend;
                            items[$count].displayed = true;
                            $count++;
                        }
                    }
                    
                    $grid.innerHTML = $grid.innerHTML + $row + i + $rowmid + $cols + $rowend;
                }
                
            };
            
            setInterval(refresh, 10000);
            
            function refresh() {
            
                for(i = 0; i < 5; i++) {
                    var newitem = items[Math.floor((Math.random() * items.length))];

                    var $randomcol = "col" + Math.floor((Math.random() * 6));

                    if(newitem.type === "image") 
                    {
                        $(document.getElementById($randomcol)).animate({height: "0", opacity: "0"}, 0);

                        document.getElementById($randomcol).innerHTML = "<p class='usernameimage'>@" + newitem.username + "</p>";
                        document.getElementById($randomcol).className = 'col-xs-4 col-fill imageitem';
                        document.getElementById($randomcol).style.backgroundImage = 'url(' + newitem.url + ')';

                        $(document.getElementById($randomcol)).animate({height: "100%", opacity: "1"}, 1000);
                    }
                    else if (newitem.type === "text")
                    {
                        $(document.getElementById($randomcol)).animate({height: "0", opacity: "0"}, 0);

                        document.getElementById($randomcol).className = "col-xs-4 col-fill twitteritem";
                        document.getElementById($randomcol).style.backgroundImage = '';
                        document.getElementById($randomcol).innerHTML =
                                "<div class='twittertext'><p>" +
                                newitem.text +
                                "</p><p class='usernametweet'>@" +
                                newitem.username +
                                "</p></div>";

                        $(document.getElementById($randomcol)).animate({height: "100%", opacity: "1"}, 1000);
                    }
                    
                }
            }
            
            function showField() {
                $(document.getElementById('sField')).fadeIn(500);
                $(document.getElementById('searchBtn')).hide();
                
                $(document.getElementById('sField')).click(function() {
                    event.stopPropagation();
                });
                
                event.stopPropagation();
            }
            
            function showOptions() {
                $(document.getElementById('optionsMenu')).fadeIn(500);
            }
            
            function hideOptions() {
                $(document.getElementById('sField')).hide();
                $(document.getElementById('optionsMenu')).fadeOut(500);
                $(document.getElementById('searchBtn')).show();
            }
            
            function hideSearchField() {
                $(document.getElementById('sField')).hide();
                $(document.getElementById('searchBtn')).fadeIn(500);
            }
			
	</script>
	          
    </head>
    
    <body style="background-color: #3d3d3d">
 
        <div class="container con-fill">

                <div class="container con-fill header" id="grid">
                </div>

                <div class="container con-fill-hor">

                    <div class="tophoverarea" onmouseover="showOptions()" onclick="hideSearchField()"></div>
                    <div class="midhoverarea" onmouseover="hideOptions()"></div>
                    <div class="bothoverarea" onmouseover=""></div>

                    <div class="row topbar" id="optionsMenu">
                        <div class="col-md-8">
                            <ol class="breadcrumb" style="background:none; margin: 0; padding: 0;">
                                <li style="font-weight: bold; color: #ebebeb;">#<?php echo $search; ?></li>
                            </ol>
                        </div>
                        <div class="col-md-4">
                            <button type="button" class="btn btn-default btn-md" id="optionsBtn"
                                    style="float:right;" onclick="">
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

            </div>
	    
    </body>
</html>

