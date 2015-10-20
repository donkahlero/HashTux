<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        
        <script type='text/javascript'>
            
            window.onload = function() {
                for(i = 0; i < 4; i++) {
                    var $rowID = "row" + i;
                    var $row = document.getElementById($rowID);
                
                    for(j = 0; j < 6; j++) {
                        $row.innerHTML = $row.innerHTML + '<div class="col-sm-2 col-fill griditem"></div>';
                    }
                }
            }
            
            function showField() {
                $(document.getElementById('sField')).show();
                $(document.getElementById('searchBtn')).hide();
                
                $(document.getElementById('sField')).click(function() {
                    event.stopPropagation();
                });
                
                event.stopPropagation();
            }
            
            $('html').click(function() {
                $(document.getElementById('sField')).hide();
                $(document.getElementById('searchBtn')).show();
            });
            
        </script>    
        
    </head>
    
    <body>
        
        <div class="container con-fill">
            
            <div class="container con-fill header">
                <div class="row-grid-3" id='row0'></div>
                <div class="row-grid-3" id='row1'></div>
                <div class="row-grid-3" id='row2'></div>
                <div class="row-grid-3" id='row3'></div>
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

