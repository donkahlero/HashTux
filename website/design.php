<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
?>

<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link href="css/bootstrap.css" rel="stylesheet">
        <link href="css/stats.css" rel="stylesheet">  
        <link href="css/hashtux.css" rel="stylesheet">
        <script src="js/userstats_fetcher.js"></script>
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        <link rel="stylesheet" type="text/css" href="//cdn.datatables.net/1.10.10/css/jquery.dataTables.css">
  
<script type="text/javascript" charset="utf8" src="//cdn.datatables.net/1.10.10/js/jquery.dataTables.js"></script>
        <script type='text/javascript'>

        var statsItems  = [];

        window.onload = function(){
            
        	 fetch("search_term_year");
        	 alert(items);
        }

        var statsList = items;

		function getItemValues(){
			   
			   for(i = 0; i < statsList.length; i++)
					{
		              itemName = statsList[i].name; 
		              itemCount = statsList[i].value;

		              var itemsEx = [itemName, itemCount];
		              
		              statsItems.push(itemsEx);
					}
			  
		   }
			
        $(document).ready(function() {

			getItemValues();
            
            $('#example').DataTable( {
                data: statsItems,
                columns: [
                    { title: "Name" },
                    { title: "Count" },
                   
                ]
            } );
        } );
        </script>    
        
    </head>
    
   <body >

    <div class="container">
        <div class="row">
					<table id="example" class="display" width="100%"></table>
		 
		</div>  			
	</div>  
    
    
 </body>
</html>

