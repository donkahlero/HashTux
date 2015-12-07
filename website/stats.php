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
        <script type="text/javascript" src="https://www.google.com/jsapi"></script>
        <script src="js/graph.js"></script>
        <script src="js/userstats_fetcher.js"></script>
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
        <script src="js/bootstrap.min.js"></script>
		<script type="text/javascript" charset="utf8" src="//cdn.datatables.net/1.10.10/js/jquery.dataTables.js"></script>
        <script type='text/javascript'>


		var termtype;
		var items = [];
		
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
		var  itemsEx = [];
		function getItemValues(){
			statsItems  = [];
			itemsEx = [];
			
			for(i = 0; i < items.length; i++)
							{
			              itemName = items[i].name; 
			              itemCount = items[i].value;
			
			              itemsEx = [itemName, itemCount];
			              
			              statsItems.push(itemsEx);
						}
				  
			   }
		
				
		function creatTable() {
				 getItemValues();
			
			 switch (termtype){
			 
			 case 'search_term_year':
                             
                             $('#tableContainer').html("<table class='searchTermTableYear' id='searchTermTableYear' width='100%'></table>");
                             
				 $('#searchTermTableYear').DataTable( {
				    	retrieve: true,
				        data: statsItems,
				       	columns: [
				        	{ title: "Top Search Terms for This Year" },
				            { title: "Count" },
				           
				     	]
				    } );
				    break; 
			 case 'search_term_month': 
                             
                             $('#tableContainer').html("<table class='searchTermTableMonth' id='searchTermTableMonth' width='100%'></table>");
				
		    	 $('#searchTermTableMonth').DataTable( {
		    		 	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Search Terms for Last Month" },
			                { title: "Count" },
			               
			            ]
			        } );
			 		break;
			 case 'search_term_week': 	
				 		
				 			$('#tableContainer').html("<table class='searchTermTableWeek' id='searchTermTableWeek' width='100%'></table>");

				$('#searchTermTableWeek').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Search Terms for Last Week" },
			                { title: "Count" },
			               
			            ]
			        } );
			        break; 
			 case 'search_term_today':
				 
							 $('#tableContainer').html("<table class='searchTermTableToday' id='searchTermTableToday' width='100%'></table>");
				 
			    $('#searchTermTableToday').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Search Terms for the Last 24H" },
			                { title: "Count" },
			               
			            ]
			        } );
			        break;
			 case 'browser_version_year':
				 
				 $('#tableContainer').html("<table class='browserTableYear' id='browserTableYear' width='100%'></table>");

					 $('#browserTableYear').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Browsers for This Year" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
			 case 'browser_version_month':
				 
				 $('#tableContainer').html("<table class='browserTableMonth' id='browserTableMonth' width='100%'></table>");

					 $('#browserTableMonth').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Browsers for Last Month" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
			 case 'browser_version_week':
					 
					 $('#tableContainer').html("<table class='browserTableWeek' id='browserTableWeek' width='100%'></table>");
	
						 $('#browserTableWeek').DataTable( {
				        	retrieve: true,
				            data: statsItems,
				            columns: [
				                { title: "Top Browsers for Last Week" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
			 case 'browser_version_today':
				 
				 $('#tableContainer').html("<table class='browserTableToday' id='browserTableToday' width='100%'></table>");

					 $('#browserTableToday').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Browsers for Last 24H" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
			 
			 case 'platform_year':
				 
				 $('#tableContainer').html("<table class='platformTableYear' id='platformTableYear' width='100%'></table>");

					 $('#platformTableYear').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Platforms This Year" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			 case 'platform_month':
				 
				 $('#tableContainer').html("<table class='platformTableMonth' id='platformTableMonth' width='100%'></table>");

					 $('#platformTableMonth').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Platforms For Last Month" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			case 'platform_week':
				 
				 $('#tableContainer').html("<table class='platformTableWeek' id='platformTableWeek' width='100%'></table>");

					 $('#platformTableWeek').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Platforms For Last Week" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			case 'platform_today':
				 
				 $('#tableContainer').html("<table class='platformTabletoday' id='platformTabletoday' width='100%'></table>");

					 $('#platformTabletoday').DataTable( {
			        	retrieve: true,
			            data: statsItems,
			            columns: [
			                { title: "Top Platforms For Last 24H" },
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
                            <li><a class="btn btn-default btn-md" id="browserButtonDay" onclick="fetch('browser_version_today')">Last 24h</a></li>
                            <li><a class="btn btn-default btn-md" id="browserButtonWeek" onclick="fetch('browser_version_week')">Last Week</a></li>
                            <li><a class="btn btn-default btn-md" id="browserButtonMonth" onclick="fetch('browser_version_month')">Last Month</a></li>
                            <li><a class="btn btn-default btn-md" id="browserButtonYear" onclick="fetch('browser_version_year')">Last Year</a></li>
                        </ul>
                    </li>
                    <li class="dropdown-submenu">
                        <a tabindex="-1" href="#">Top Platforms</a>
                        <ul class="dropdown-menu">
                            <li><a class="btn btn-default btn-md" id="platformButtonDay" onclick="fetch('platform_today')">Last 24h</a></li>
                            <li><a class="btn btn-default btn-md" id="platformButtonWeek" onclick="fetch('platform_week')">Last Week</a></li>
                            <li><a class="btn btn-default btn-md" id="platformButtonMonth" onclick="fetch('platform_month')">Last Month</a></li>
                            <li><a class="btn btn-default btn-md" id="platformButtonYear" onclick="fetch('platform_year')">Last Year</a></li>
                        </ul>
                    </li>
                </ul>
            </div>
        </div>
        
    </div>
    
    <div class="container">
        <div class="row">
            <div class="col-md-6" id="tableContainer"></div>
            	<div class="col-md-6" id="graphContainer">
                	<div class="container-fluid" align="center" width="100%" id=""chart_div"">
                </div> 
            </div>
            
        </div>
    </div> 
    
</body>
</html>
 
