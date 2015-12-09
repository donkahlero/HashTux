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
        <link rel="stylesheet" type="text/css" href="//cdn.datatables.net/1.10.10/css/jquery.dataTables.css">
  
<script type="text/javascript" charset="utf8" src="//cdn.datatables.net/1.10.10/js/jquery.dataTables.js"></script>
        <script type='text/javascript'>

        var statsItems  = [];

        function statsObj(name,value) {
        	  this.name = name;
        	  this.value = value; 
        	}

        var statsList = [new statsObj("hashtux",31),
        	                    new statsObj("Summer",12),
        	                    new statsObj("Sunny",13),
        	                    new statsObj("Candy",14),
        					    new statsObj("Hash",6),
        					    new statsObj("tux",9),
        						new statsObj("lol",10),
        						new statsObj("Pepperoni",2),
        						new statsObj("beautiful",5),
        						new statsObj("beauty",5)
        					   ];

		function getItemValues(){
			   
			   for(i = 0; i < statsList.length; i++)
					{
		              itemName = statsList[i].name; 
		              itemCount = statsList[i].value;

		              var items = [itemName, itemCount];
		              
		              statsItems.push(items);
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

