/** 
 * Fetch and display trending terms info from HashTux
 */
function loadTrendingHT() {
	$.ajax({
		url: "/ajax_post.php?search=search_term_week",
		type: "post",
		data: JSON.stringify({request_type:"stats"}),

		success: function (trendingJSON) {
			data = JSON.parse(trendingJSON); 

			result = "";

			// Sort search terms by their popularity
			data = data.sort(compare);

			// Concatenate together some HTML with 5 links to the most popular searches
			for (i = 0; i < data.length && i < 5; i++) {
				result += "<a class='darkgreytext' href='" + data[i].key + "'>" + data[i].key + "</a><br />";	
			}

			$("#trending").html(result);
		}
	});
}


/** 
 * Comparator for comparing trending terms from HashTux (by value, which is search 
 * occurences/popularity)
 */
function compare(item1, item2) {
	// All other browsers than Firefox need a 1, 0 or -1 as return value here
	if (item1.value < item2.value) {
		return 1;
	} 
	return -1;	
}


/** 
 * Fetch and display suggestions based on what's trending on twitter
 */
function loadTrendingTwitter() {
	$.ajax({
		url: "/twitter_popular.php",
		type: "get",

		success: function (trendingTwitterJSON) {
			data = JSON.parse(trendingTwitterJSON);

			result = "";

			// The first object has an array stored as the value of key "trends"
			trends = data[0].trends;
			count = 0;

			// Check until we have found 5 items, or until we run out of trends
			for (i = 0; i < trends.length && count < 5; i++) {
				// Only include those that contain western characters.
				if (contains_legal_characters(trends[i].name)) {
					currentTrend = trends[i].name.replace("#", "");
					result += "<a class='darkgreytext' href='" + currentTrend + "'>" + currentTrend + "</a><br />";	
					count++;
				}
			}
			$("#trending-twitter").html(result);
		}
	});
}



