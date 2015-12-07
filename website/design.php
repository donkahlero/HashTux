<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
?>
<html lang="en">
    <head>
        
        <title>HashTux</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link rel="stylesheet" type="text/css" href="//cdn.datatables.net/1.10.10/css/jquery.dataTables.css">
        <link href="css/bootstrap.css" rel="stylesheet">
        <link href="css/stats.css" rel="stylesheet">  
        <link href="css/hashtux.css" rel="stylesheet">
        <script src="js/userstats_fetcher.js"></script>
        <script type="text/javascript" src="https://www.google.com/jsapi"></script>
        <script src="js/graph.js"></script>
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
		<script type="text/javascript" charset="utf8" src="//cdn.datatables.net/1.10.10/js/jquery.dataTables.js"></script>
        <script type='text/javascript'>


		var termtype;
		
		
		function fetch(term) {
		    items = [];
		    var options = {request_type: "stats", options: []};
		    $.ajax({
		        url: "/ajax_post.php?search=" + term,
		        type: "post",                    
		        data: JSON.stringify(options),
		                    
		        success: function (myString) { 
		            termtype = term;
		            parse_to_items(myString);
		            creatTable();
		            
		        }
		    });
		}
		
		
		var statsItems  = [];
		var searchTermTableY = $('#searchTermTableYear').DataTable();
		function getItemValues(){
			   for(i = 0; i < items.length; i++)
					{
		              itemName = items[i].name; 
		              itemCount = items[i].value;
		
		              var itemsEx = [itemName, itemCount];
		              
		              statsItems.push(itemsEx);
					}
			  
		   }
		
				
		function creatTable() {
			getItemValues();
		 switch (termtype){
		 
		 case 'search_term_year':
			 $('#searchTermTableYear').DataTable( {
			    	retrieve: true,
			        data: statsItems,
			       	columns: [
			        	{ title: "Top search Terms for This Year" },
			            { title: "Count" },
			           
			     	]
			    } );
			    break; 
		 case 'search_term_month': 
			//$( "div" ).remove( ".searchTermTableY" );
	    	 $('#searchTermTableMonth').DataTable( {
	    		 	retrieve: true,
		            data: statsItems,
		            columns: [
		                { title: "Top search Terms for Last Month" },
		                { title: "Count" },
		               
		            ]
		        } );
		 		break;
		 case 'search_term_week':
			//$( "div" ).remove( ".searchTermTableY" );
		        $('#searchTermTableWeek').DataTable( {
		        	retrieve: true,
		            data: statsItems,
		            columns: [
		                { title: "Top search Terms for Last Week" },
		                { title: "Count" },
		               
		            ]
		        } );
		        break; 
		 case 'search_term_today':
			//$( "div" ).remove( ".searchTermTableY" );
		        $('#searchTermTableToday').DataTable( {
		        	retrieve: true,
		            data: statsItems,
		            columns: [
		                { title: "Top search Terms for Last 24H" },
		                { title: "Count" },
		               
		            ]
		        } );
		        break;
		 case 'browser_version_year':
			 $('#browserTableYear').DataTable( {
		        	retrieve: true,
		            data: statsItems,
		            columns: [
		                { title: "Top Browsers for This Year" },
		                { title: "Count" },
		               
		            ]
		        } );
		        break;
			 
		 }
		       
		}
		     
    </script>
    
</head>

<body>
    
    <div>
        <h1 align="center">#hashtux</h1>
        <h2 align="center">Welcome to HashTux User Habits Statistics</h2>
    </div>
    
    <div class="container" align="center">
        <div class="input-group" style="float:left;" >
            <div class="dropdown">
                <button class="btn btn-primary dropdown-toggle" type="button" data-toggle="dropdown">Select a Field to View  
                    <span class="caret"></span></button>
                <ul class="dropdown-menu multi-level" role="menu" aria-labelledby="dropdownMenu">
                    <li class="dropdown-submenu">
                        <a tabindex="-1" href="#">Top Search Terms&nbsp;&nbsp;</a>
                        <ul class="dropdown-menu">
                            <li><a  class="btn btn-default btn-md" id="search_termButtonDay" onclick="fetch('search_term_today')">Last 24h</a></li>
                            <li><a  class="btn btn-default btn-md" id="search_termButtonWeek" onclick="fetch('search_term_week')">Last Week</a></li>
                            <li><a 	class="btn btn-default btn-md" id="search_termButtonMonth" onclick="fetch('search_term_month')">Last Month</a></li>
                            <li><a 	class="btn btn-default btn-md" id="search_termButtonYear" onclick="fetch('search_term_year')">Last Year</a></li>
                        </ul>
                    </li>
                    <li class="dropdown-submenu">
                        <a tabindex="-1" href="#">Top Browsers</a>
                        <ul class="dropdown-menu">
                            <li><a class="btn btn-default btn-md" id="browserButtonDay" onclick="">Last 24h</a></li>
                            <li><a class="btn btn-default btn-md" id="browserButtonWeek" onclick="">Last Week</a></li>
                            <li><a class="btn btn-default btn-md" id="browserButtonMonth" onclick="">Last Month</a></li>
                            <li><a class="btn btn-default btn-md" id="browserButtonYear" onclick="fetch('browser_version_year')">Last Year</a></li>
                        </ul>
                    </li>
                    <li class="dropdown-submenu">
                        <a tabindex="-1" href="#">Top Platforms</a>
                        <ul class="dropdown-menu">
                            <li><a class="btn btn-default btn-md" id="platformButtonDay" onclick="">Last 24h</a></li>
                            <li><a class="btn btn-default btn-md" id="platformButtonWeek" onclick="">Last Week</a></li>
                            <li><a class="btn btn-default btn-md" id="platformButtonMonth" onclick="">Last Month</a></li>
                            <li><a class="btn btn-default btn-md" id="platformButtonYear" onclick="">Last Year</a></li>
                        </ul>
                    </li>
                </ul>
            </div>
        </div>
        
    </div>
      
    <div class="container">
        <div class="row">
            <div class="col-md-6">
	            <div  class="searchTermTableY">
	                 <table class="searchTermTableYear" id="searchTermTableYear"	width="100%"></table>
	            </div>
		        	<div  class="searchTermTableM">
	                	<table id="searchTermTableMonth"	width="100%"></table>
	             	</div>
			        	<div  class="searchTermTableW">
		                	<table id="searchTermTableWeek"	width="100%"></table>
		             	</div>
			             	<div  class="searchTermTableT">
			                	<table id="searchTermTableToday" width="100%"></table>
			             	</div>
			             		<div  class="browserTableY">
			                	<table id="browserTableYear" width="100%"></table>
			             	</div>
		   
            </div>
            
            <div class="col-md-6">
            <button type="button" class="btn btn-default btn-md" id="reload"
                                style="float:right; margin-right: 15px;" onclick="removeT()">
                            remove
                        </button>
                <!--Div that will hold the pie chart-->
                <div class="container-fluid" align="center" width="100%" id=""chart_div"">
                </div> 
            </div>
            
        </div>
    </div> 
    
</body>
</html>
 