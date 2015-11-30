// Load the Visualization API and the piechart package.
      google.load('visualization', '1.0', {'packages':['corechart', 'bar']});

      // Set a callback to run when the Google Visualization API is loaded.
      google.setOnLoadCallback(drawChart);

      // Callback that creates and populates a data table,
      // instantiates the pie chart, passes in the data and
      // draws it.
      function drawChart() {

        // Create the data table.
        var data = new google.visualization.DataTable();
        data.addColumn('string', 'Hashtag');
        data.addColumn('number', 'Times searched');
        data.addRows(getStats());

        // Set chart options
        var options = {
            'title': getTitle(),
            'width':600,
            'height':400,
            animation: {"startup":true, "duration":5000}
        };

        // Instantiate and draw our chart, passing in some options.
        var chart = new google.visualization.BarChart(document.getElementById('chart_div'));
        chart.draw(data, options);
        
        function getTitle($Name) {
            $Name = 'NameOfChart';
            return $Name;
        }
        
        function getStats() {
            var list = [
          ['Hashtux', 431],
          ['Summer', 354],
          ['Sun', 489],
          ['Candy', 98],
          ['Pepperoni', 30],
          ['Dennis', 170],
          ['Hashtux', 431],
          ['Summer', 354],
          ['Sun', 489],
          ['Lisaann', 1000],
          ['Candy', 98],
          ['Pepperoni', 30],
          ['Dennis', 170]
        ];
        return list;
        }
      }

