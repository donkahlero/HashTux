<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link href="css/bootstrap.css" rel="stylesheet">
        <link href="css/stats.css" rel="stylesheet">  
        <link href="css/hashtux.css" rel="stylesheet">
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
        <script type='text/javascript'>

        var no = ["1","2","3","4"];
        var searchTerms = ["Roger","Steve","Mike","Charlie"];
        var value = ["8","7","6","2"];


        tablegenerate (no,searchTerms,value);

        function tablegenerate (no,searchTerms,value) {
            for(i=0; i<no.length;i++)
            {
                 var $formrow = '<tr><td>'+no[i]+'</td><td>'+searchTerms[i]+'</td><td>'+value[i]+'</td></tr>';
                $('.searchTermTable').append($formrow);
            }
        }
          
        </script>    
        
    </head>
    
   <body >

    <div class="container">
					
		<table table class="searchTermTable" style="float:justify;">
    		<tr>
      			<th width="10%">No.</th>
        		<th width="80%">Search Term</th>
       			<th width="10%">Value</th>
    		</tr>
		</table>
	</div>  
    
    
 </body>
</html>

