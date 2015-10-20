<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        
        <link href="css/bootstrap.css" rel="stylesheet">
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        
        <script>
            function showField() {
                $('sField').show();
            }
        </script>    
        
    </head>
    
    <body>
        
        <div class="container con-fill" style="margin-top:15px">
            <div class="row">
                <div class="col-md-4"></div>
                <div class="col-md-4"></div>
                <div class="col-md-4">
                    <input type="text" class="form-control" id="sField" 
                           name="sField" style="display: none;" />
                    <button type="button" class="btn btn-default btn-md" 
                            style="float:right" onclick="showField()">
                        Search
                    </button>
                </div>
            </div>
        </div>
        
    </body>
</html>

