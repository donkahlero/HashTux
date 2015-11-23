<?php
	/* Start a new session or resume if the client has a cookie ;) */
	session_start();
?>
<!DOCTYPE html>
<html>
<head>
    <title>HashTux Stats</title>
<link href="http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css" rel="stylesheet">   
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>
<link rel="stylesheet" 
href="http://cdn.datatables.net/1.10.2/css/jquery.dataTables.min.css"></style>
<script type="text/javascript" 
src="http://cdn.datatables.net/1.10.2/js/jquery.dataTables.min.js"></script>
<script type="text/javascript" 
src="http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"></script>
</head>
<body>
    <div class="container">
        <h1>User Habits </h1>
       <table table class= "table table-hover" id="myTable" >  
		<thead>  
		  <tr>  
		    <th>Search Term</th>  
		    <th>Browser</th>  
		    <th>Platform</th>
		    <th>Country</th>  
		    <th>Language</th>  
		  </tr>  
		</thead>  
		<tbody>  
		  <tr>  
		    <td>tux</td>
		    <td>Firefox</td>   
		    <td>Windows</td>  
		    <td>India</td>  
		    <td>Hindu</td>  
		  </tr>  
		  <tr>  
		    <td>hashtux</td> 
		    <td>Safari</td>    
		    <td>Linux</td>  
		    <td>United Kingdom</td>  
		    <td>English</td>  
		  </tr>  
		</tbody>  
	      </table>
	      <div class="table-responsive">
			<table id="myTable" class="display table" width="100%" >
				</table>
					</div>
	       	</div>
	</div>
	      
		 
	<script>
	$(document).ready(function(){
	    $('#myTable').dataTable();
	});</script>
</body>
</html>