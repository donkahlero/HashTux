<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        
        <script type='text/javascript'>
            
            var pic = {url:"", shown:false, frozen:false};
            
            var images = ["http://i.imgur.com/JfKwovX.jpg", "http://i.imgur.com/PLehguA.jpg", 
                        "http://i.imgur.com/bHkmaqm.jpg", "http://i.imgur.com/DfjlogK.jpg",
                        "http://i.imgur.com/JgNFQlc.jpg", "http://i.imgur.com/0OYdzge.jpg",
                        "http://i.imgur.com/iBnXFAA.jpg", "http://i.imgur.com/HiqYpKC.jpg",
                        "http://i.imgur.com/aFvyTTp.jpg", "http://i.imgur.com/fDutob0.jpg",
                        "http://i.imgur.com/xx7hFLa.jpg", "http://i.imgur.com/rRK1c1O.jpg",
                        "http://i.imgur.com/JfKwovX.jpg", "http://i.imgur.com/PLehguA.jpg", 
                        "http://i.imgur.com/bHkmaqm.jpg", "http://i.imgur.com/DfjlogK.jpg",
                        "http://i.imgur.com/JgNFQlc.jpg", "http://i.imgur.com/0OYdzge.jpg",
                        "http://i.imgur.com/iBnXFAA.jpg", "http://i.imgur.com/HiqYpKC.jpg",
                        "http://i.imgur.com/aFvyTTp.jpg", "http://i.imgur.com/fDutob0.jpg",
                        "http://i.imgur.com/xx7hFLa.jpg", "http://i.imgur.com/rRK1c1O.jpg"];
            
            window.onload = function() {
                
                var $grid = document.getElementById('grid');
                var $row = "<div class='row-grid-3'>";
                var $rowend = "</div>";
                var $colstart = "<div class='col-xs-2 col-fill griditem' style='background-image:url(";
                var $colmid = ");' id='col";
                var $colend = "'></div>";
                var $count = 0;
                
                for(i = 0; i < 4; i++) {
                    
                    var $cols = "";
                    
                    for(j = 0; j < 6; j++) {
                        $cols = $cols + $colstart + images[$count] + $colmid + $count + $colend;
                        $count++;
                    }
                    $grid.innerHTML = $grid.innerHTML + $row + $cols + $rowend;
                }                
                
            };
            
            function refresh() {
                var url = images[Math.floor((Math.random() * 23))];
                
                var $randomcol = "col" + Math.floor((Math.random() * 23));
                
                $(document.getElementById($randomcol)).fadeOut(500);
                document.getElementById($randomcol).style.backgroundImage = 'url(' + url + ')';
                $(document.getElementById($randomcol)).fadeIn(500);
            }
            
            function showField() {
                $(document.getElementById('sField')).fadeIn(500);
                $(document.getElementById('searchBtn')).hide();
                
                $(document.getElementById('sField')).click(function() {
                    event.stopPropagation();
                });
                
                event.stopPropagation();
            }
            
            $('html').click(function() {
                $(document.getElementById('sField')).fadeOut(500);
                $(document.getElementById('searchBtn')).show();
                refresh();
            });
            
        </script>    
        
    </head>
    
    <body style="background-color: #FFF">
        
        <div class="container con-fill">
            
            <div class="container con-fill header" id="grid">
            </div>
        
            <div class="container con-fill-hor">
                <div class="row">
                    <div class="col-md-4"></div>
                    <div class="col-md-4"></div>
                    <div class="col-md-4">
                        <div class="input-group" style="display: none; margin-top: 15px;" id="sField">
                            <span class="input-group-addon">#</span>
                            <input type="text" class="form-control" name="sField">
                        </div>
                        <button type="button" class="btn btn-default btn-md" id="searchBtn"
                                style="float:right; margin-top: 15px;" onclick="showField()">
                            Search
                        </button>
                    </div>
                </div>
            </div>
            
        </div>
        
    </body>
</html>

