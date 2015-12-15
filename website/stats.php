<?php
/* Start a new session or resume if the client has a cookie ;) */
session_start ();
?>
<html lang="en">
<head>

<title>HashTux</title>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<link rel="stylesheet" type="text/css"
	href="//cdn.datatables.net/1.10.10/css/jquery.dataTables.css">
<link href="css/bootstrap.css" rel="stylesheet">
<link href="css/stats.css" rel="stylesheet">
<link href="css/hashtux.css" rel="stylesheet">
<script src="js/userstats_fetcher.js"></script>
<script type="text/javascript" src="https://www.google.com/jsapi"></script>
<script
	src="https://ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
<script src="js/bootstrap.min.js"></script>
<script type="text/javascript" charset="utf8"
	src="//cdn.datatables.net/1.10.10/js/jquery.dataTables.js"></script>
<script type='text/javascript'>


		var termtype; //	The type of statstics (browser,search_term,platform)
		var type = "search_term";
		var period = "year"; 
		var items = []; //	An array to store all items fetched
		
		//	A function for any future requests. Uses ajax_post.php to fetch the
        //	JSON object from the http server.
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
		//	This function is used to determine the type of statstics to be requested.
		//	and run fetch function with the specific period.
		function changeType(typeTerm){
			
			type = typeTerm;
			
            fetch(type + '_' + period);
            
			}

		//	This function is used to determine the period of statstics to be requested.
		//	and run fetch function with the specific type.
		function changePeriod(periodTerm){
			
			period = periodTerm; 
			fetch(type + '_' + period);
			
			}
		
		var statsItems  = [];//	An array to store all fetched itmes 
		var  itemsEx = [];	//	An array to store an array of itmes (itemName) and(itemCount). 
		//	This function is used to put the JSON object in items array in to a new array statsItems.
		function getItemValues(){
			statsItems  = [];	//	A new array to store the JSON objects in a list to be used in creatTable().
			itemsEx = [];		//	Array of lists of items.name and items.value.  		
			
			for(i = 0; i < items.length; i++)
							{
			              itemName = items[i].name; 
			              itemCount = items[i].value;
			              itemIndex  = [];
			              itemSum  = [items.length];
			              itemsEx = [itemIndex,itemName,itemCount,itemSum];
			              
			              statsItems.push(itemsEx);
						}
				  
			   }
		
		window.onload =  fetch('search_term_week');	//	search term of the year is shown to start with when the page loads.
		//	creatTable is function used to creat tables, Each table is created using 
		//	a library called DataTable. Multiple case statments are used to creat the diffrent tables. 
		function creatTable() {
				 getItemValues();
			//	This is the switch statement with termtype as a case to check for diffrent terms requested.  	
			switch (termtype){

			 case 'search_term_year':
				 	 // Here the table for search term year is created in tableContainer           
               $('#tableContainer').html("<table class='searchTermTableYear' id='searchTermTableYear' width='100%'></table>");
           			 //	Here the table for search term year is rendered using DataTable
           			 document.getElementById("test").innerHTML = "Sum of search terms =" +" "+ itemSum;
					 $('#searchTermTableYear').DataTable( {
					    	retrieve: true, //	retrieve is a boolean that allows the table to be rendered after initializing. 
					    	"aaSorting": [[2,'desc'], ],//	aaSorting sorts the count in descending order. 
					    	"iDisplayLength": 50,     
					        "fnDrawCallback": function ( oSettings ) {
					            /* Need to redo the counters if filtered or sorted */
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
					        data: statsItems, // The table is generated using data from statsItems array. 
					       	columns: [		  // colums assigns the titles for the table. 	 
					       	    { title: "Index"},																		
					        	{ title: "Top search terms for this year"},
					            { title: "Count" },
					          
					     	] 
					    } );
					    break; 
			 case 'search_term_month': 
                             
                $('#tableContainer').html("<table class='searchTermTableMonth' id='searchTermTableMonth' width='100%'></table>");
				
			    	 $('#searchTermTableMonth').DataTable( {
			    		 	retrieve: true,
			    		 	"zeroRecords": "No matching records found",
			    		 	"aaSorting": [[2,'desc']],
			    		 	"iDisplayLength": 50,
			    		 	 "fnDrawCallback": function ( oSettings ) {
						            if ( oSettings.bSorted || oSettings.bFiltered )
						            {
						                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
						                {
						                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
						                }
						            }
						        },
				            data: statsItems,
				            columns: [ 
								{ title: "Index"},	
				                { title: "Top search terms for last month" },
				                { title: "Count" },
				               
				            ]
				        } );
				 		break;
			 case 'search_term_week': 	
				 		
				$('#tableContainer').html("<table class='searchTermTableWeek' id='searchTermTableWeek' width='100%'></table>");

					$('#searchTermTableWeek').DataTable( {
				        	retrieve: true,
				        	"zeroRecords": "No matching records found",
				        	"aaSorting": [[2,'desc']],
			    		 	"iDisplayLength": 50,
			    		 	 "fnDrawCallback": function ( oSettings ) {
						            if ( oSettings.bSorted || oSettings.bFiltered )
						            {
						                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
						                {
						                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
						                }
						            }
						        },
				            data: statsItems,
				            "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
				            columns: [
								{ title: "Index"},	       
				                { title: "Top search terms for last week" },
				                { title: "Count" },
				               
				            ]
				        } );
				        break; 
			 case 'search_term_today':
				 
				$('#tableContainer').html("<table class='searchTermTableToday' id='searchTermTableToday' width='100%'></table>");
				 
				    $('#searchTermTableToday').DataTable( {
				        	retrieve: true,
				        	"zeroRecords": "No matching records found",
				        	"aaSorting": [[2,'desc']],
			    		 	"iDisplayLength": 50,
			    		 	 "fnDrawCallback": function ( oSettings ) {
						            if ( oSettings.bSorted || oSettings.bFiltered )
						            {
						                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
						                {
						                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
						                }
						            }
						        },
				            data: statsItems,
				            "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
				            columns: [
								{ title: "Index"},	
				                { title: "Top search terms for the last 24 Hours" },
				                { title: "Count" },
				               
				            ]
				        } );
				        break;
			 case 'browser_version_year':
				 
				 $('#tableContainer').html("<table class='browserVersionTableYear' id='browserVersionTableYear' width='100%'></table>");

					 $('#browserVersionTableYear').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top browsers with version for this year" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
			 case 'browser_version_month':
				 
				 $('#tableContainer').html("<table class='browserVersionTableMonth' id='browserVersionTableMonth' width='100%'></table>");

					 $('#browserVersionTableMonth').DataTable( {
			        	retrieve: true,
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			        	"iDisplayLength": 50,
			            data: statsItems,
			            columns: [
							{ title: "Index"},	        
			                { title: "Top browsers with version for last month" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
			 case 'browser_version_week':
					 
					 $('#tableContainer').html("<table class='browserVersionTableWeek' id='browserVersionTableWeek' width='100%'></table>");
	
						 $('#browserVersionTableWeek').DataTable( {
				        	retrieve: true,
				        	"zeroRecords": "No matching records found",
				        	"aaSorting": [[2,'desc']],
			    		 	"iDisplayLength": 50,
			    		 	 "fnDrawCallback": function ( oSettings ) {
						            if ( oSettings.bSorted || oSettings.bFiltered )
						            {
						                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
						                {
						                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
						                }
						            }
						        },
				            data: statsItems,
				            columns: [
								{ title: "Index"},	
				                { title: "Top browsers with version for last week" },
				                { title: "Count" },
				               
				            ]
				        } ); 
					    break;
			 case 'browser_version_today':
				 
				 $('#tableContainer').html("<table class='browserVersionTableToday' id='browserVersionTableToday' width='100%'></table>");

					 $('#browserVersionTableToday').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	       
			                { title: "Top browsers with version for the last 24 Hours" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
			 case 'browser_year':
				 
				 $('#tableContainer').html("<table class='browserTableYear' id='browserTableYear' width='100%'></table>");

					 $('#browserTableYear').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top browsers for this year" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
				case 'browser_month':
				 
				 $('#tableContainer').html("<table class='browserTableMonth' id='browserTableMonth' width='100%'></table>");

					 $('#browserTableMonth').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	       
			                { title: "Top browsers for last month" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
				case 'browser_week':
				 
				 $('#tableContainer').html("<table class='browserTableWeek' id='browserTableWeek' width='100%'></table>");

					 $('#browserTableWeek').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top browsers for last week" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
				case 'browser_today':
				 
				 $('#tableContainer').html("<table class='browserTableToday' id='browserTableToday' width='100%'></table>");

					 $('#browserTableToday').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	         
			                { title: "Top browsers for the last 24 Hours" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
			 
			 case 'platform_year':
				 
				 $('#tableContainer').html("<table class='platformTableYear' id='platformTableYear' width='100%'></table>");

					 $('#platformTableYear').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms this year" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			 case 'platform_month':
				 
				 $('#tableContainer').html("<table class='platformTableMonth' id='platformTableMonth' width='100%'></table>");

					 $('#platformTableMonth').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms for last month" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			case 'platform_week':
				 
				 $('#tableContainer').html("<table class='platformTableWeek' id='platformTableWeek' width='100%'></table>");

					 $('#platformTableWeek').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms for last week" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			case 'platform_today':
				 
				 $('#tableContainer').html("<table class='platformTabletoday' id='platformTabletoday' width='100%'></table>");

					 $('#platformTabletoday').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms for the last 24 Hours" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			case 'platform_browser_year':
				 
				 $('#tableContainer').html("<table class='platformBrowserTable' id='platformBrowserTable' width='100%'></table>");

					 $('#platformBrowserTable').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms use with browsers for this year" },
			                { title: "Count" },
			                 
			            ]
			        } ); 
				    break;
 			case 'platform_browser_month':
				 
				 $('#tableContainer').html("<table class='platformBrowserMonth' id='platformBrowserMonth' width='100%'></table>");

					 $('#platformBrowserMonth').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms use with browsers for last month" },
			                { title: "Count" },
			                
			            ]
			        } ); 
				    break;
 			case 'platform_browser_week':
				 
				 $('#tableContainer').html("<table class='platformBrowserWeek' id='platformBrowserWeek' width='100%'></table>");

					 $('#platformBrowserWeek').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms use with browsers for last week" },
			                { title: "Count" },
			               
			            ]
			        } ); 
				    break;
 			case 'platform_browser_today':
				 
				 $('#tableContainer').html("<table class='platformBrowserDay' id='platformBrowserDay' width='100%'></table>");

					 $('#platformBrowserDay').DataTable( {
			        	retrieve: true,
			        	"zeroRecords": "No matching records found",
			        	"aaSorting": [[2,'desc']],
		    		 	"iDisplayLength": 50,
		    		 	 "fnDrawCallback": function ( oSettings ) {
					            if ( oSettings.bSorted || oSettings.bFiltered )
					            {
					                for ( var i=0, iLen=oSettings.aiDisplay.length ; i<iLen ; i++ )
					                {
					                    $('td:eq(0)', oSettings.aoData[ oSettings.aiDisplay[i] ].nTr ).html( i+1 );
					                }
					            }
					        },
			            data: statsItems,
			            columns: [
							{ title: "Index"},	
			                { title: "Top platforms use with browser for the last 24 Hours" },
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
		<h2 align="center">Welcome to HashTux User Habits Statistics</h2>
	</div>
	<div class="container">
		<nav class="navbar navbar-inverse">
			<div class="container-fluid">
				<div class="navbar-header">
					<a class="navbar-brand" href=""></a>
				</div>
				<div>
					<ul class="nav navbar-nav ">
						<li><a class="btn" onclick="changeType('search_term')">Search Term</a></li>
						<li><a class="btn" onclick="changeType('browser')">Browser</a></li>
						<li><a class="btn" onclick="changeType('platform')">Platform</a></li>
						<li class="dropdown"><a class="dropdown-toggle"
							data-toggle="dropdown" href="#">Combinations
								<ul class="dropdown-menu">
									<li><a class="btn" onclick="changeType('platform_browser')">Platform/Browser</a></li>
									<li><a class="btn" onclick="changeType('browser_version')">Browser/Version</a></li>
								</ul></li>
					</ul>
					<ul class="nav navbar-nav navbar-right">
						<li class="dropdown"><a class="dropdown-toggle"
							data-toggle="dropdown" href="#">Period <span class="caret"></span></a>
							<ul class="dropdown-menu">
								<li class="btn" onclick="changePeriod('today')">Last 24 Hours</li>
								<li	class="btn" onclick="changePeriod('week')">Last Week</li>
								<li	class="btn" onclick="changePeriod('month')">Last Month</li>
								<li	class="btn" onclick="changePeriod('year')">Last Year</li>
							</ul>
					</ul>
				</div>
			</div>
	
	</div>
	<div class="container">
				<div class="row" id="tableContainer"></div>
						<div class="space"id="test"></div>
	</div>
	</header>
</body>
</body>
</html>