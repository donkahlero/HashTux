<?php
/**
 * This file is responsible for handing off the search to the erlang backend.
 * Usage: generate request options with build_request_options() if you want to.
 * Make the request with request().
 *
 * We use cURL for the actual request.
 * We pass some options and details that we store as "user habit data",
 * see further comments below and in documentation on the format.
 *
 * NOTE: Make sure this file does not have ANY text/characters outside the php tags!
 */


require_once ("conf/config.php");


/**
 * This simple datatype class will wrap the information needed to keep track of
 * backend servers, and methods for comparing.
 * No heavy encapsulation motivated here.
 * @author Jerker
 */
class backend_server {
	var $address;
	var $config_priority;
	var $down_timestamp;
	
	function __construct($address, $config_priority, $down_timestamp) {
		$this->address = $address;
		$this->config_priority = $config_priority;
		$this->down_timestamp = $down_timestamp;
	}

	/**
	 * The easiest way to compare these objects for "equality" is to look at the string
	 * representations of the fields that matter. This is used to verify if the file with
	 * known downtime has all (and only) the servers from the config file. 
	 */
	public function __toString()
	{
		return $this->address . "|" . $this->config_priority;	
	}
	
	
	/**
	 * Used for sorting the server array. Sort the array in ascending order - servers with no 
	 * known instances of being down or downtime long ago (likely up again) will be further up.
	 * On equal downtime, the server declared earlier in the config file is preferred.
	 * @param $server1 The first backend_server object
	 * @param $server2 The second backend_server object
	 */	
	public static function compare($server1, $server2) {
		if ($server1->down_timestamp == $server2->down_timestamp) {
			// Equal downtime - compare priority in config file
			return $server1->config_priority > $server2->config_priority;  
		}
		// The normal case: compare by timestamps of last known downtime.
		return $server1->down_timestamp > $server2->down_timestamp;
	}
}


/**
 * This class maintains an ordered list of backend servers. When we first ask for the
 * preferred server, it gives us the topmost entry in the backend_servers array in the
 * config. However, if requests to this server ever times out or has problems, the using
 * code can call preferred_server_down() to tell this module to adjust the order of the
 * list of backend servers. The list will be stored to file between executions of this
 * script.
 * 
 * @author jerker
 *
 */
class server_manager {
	var $server_array = [];
	var $servers_tried = 1;
	var $server_filename = "servers.order";
	
	
	/**
	 * Constructor. 
	 */
	function __construct() {
		global $config;
		
		// Read the file into an array if posisble.
		$this->server_array = $this->load_file();

		if ($this->server_array != false) {
			// Reading the file went OK. Now just clear out incidents older than our treshold.
			$this->clear_old_incidents();
			
			// Then see if the order should be changed.
			$this->reevaluate_order();			
		} else {			
			// There was trouble with the file, load_file has returned false.			
			// Read all the servers from the config file
			$this->server_array = $this->get_configured_servers();
			
			// ...and save.
			$this->save_file();			
		} 
	}
	
	
	/**
	 * Return the preferred server, if the list contains at least one entry and the using
	 * code hasn't gone through all the available servers (during the lifetime of this 
	 * object), otherwise false.
	 */
	public function get_pref_server_addr() {
		$pref_server_addr = false;
		
		if (sizeof($this->server_array) > 0 && 
				$this->servers_tried <= sizeof($this->server_array)) {
					
			// Take the address from the server object in position 0 in the array
			$pref_server_addr = $this->server_array[0]->address;
		}
		
		return $pref_server_addr;
	}
	
	
	/**
	 * Call this if there are problems with the preferred server.
	 */
	public function preferred_server_down() {
		// Increase how many servers we've tried so far (during the lifetime of this object)
		$this->servers_tried++;	

		// Put the current timestamp as the value for the current server
		if (sizeof($this->server_array) > 0 && is_array($this->server_array)) {		
			$this->server_array[0]->down_timestamp = time();
		}

		// Reevaluate the preferred order of servers.
		$this->reevaluate_order();
		
		// Write to the servers.order file
		$this->save_file();	
	}
	
	
	/**
	 * Reevaluate the order in which the servers are preferred.  
	 */
	private function reevaluate_order() {
		// Sort the server array. Uses the static compare function in the backend_server class.
		usort($this->server_array, array('backend_server','compare'));
	}
	
	
	/**
	 * Forget about any known instances of servers being down that are older than
	 * backend_reconsider_time (seconds) in the config. 
	 */
	private function clear_old_incidents() {
		global $config; 
		
		foreach ($this->server_array as $currentserver) {
			// Only check if there is actually a value other than 0
			if ($currentserver->down_timestamp > 0 &&
					$currentserver->down_timestamp < time() - $config['backend_reconsider_time']) {
				// Clear the value to 0
				$currentserver->down_timestamp = 0;
			}
		}		
	}
	
	
	/**
	 * Reads the servers declared in the config file into an array.
	 */
	private function get_configured_servers() {
		global $config; 
		$config_servers = [];
			
		// Let's create the server array based on the configuration
		foreach ($config['backend_servers'] as $index => $currentserver) {
			// Add each server with a known latest downtime of 0
			array_push($config_servers,
					new backend_server($currentserver, $index, 0));
		}
		return $config_servers;
	}
	
	
	/**
	 * Read from the servers.order file and see if it exists and contains all the servers.
	 * If so, return the array. If not, return false.
	 */
	private function load_file() {
		$result = false;
		if (file_exists($this->server_filename)) {
			// Read the file where the array is stored in it's serialized form.
			// Unserialize to get it as an array again 
			$array_result = unserialize(file_get_contents($this->server_filename));
			
			// Only return the loaded array if all entries match the config
			if ($this->has_configured_servers($array_result)) {
				$result = $array_result;				
			}
		}
		return $result;
	}
	
	
	/**
	 * Make sure all servers from the config (and ONLY those) are present in the array
	 * given as the argument. 
	 */
	private function has_configured_servers($proposed_server_array) {
		global $config;
		
		if (is_array($proposed_server_array)) {
			$configured_servers = $this->get_configured_servers();
			
			// See which values in $proposed_servers are also in the config.
			// array_instersect checks if the string representations of elements are equal
			// see the comment regarding __tostring() in the backend_server class.
			$common = array_intersect($proposed_server_array, $configured_servers);
			
			// If the size of the intersection array is the same as the amount of servers
			// in the config, we are sure the proposed array contains the right servers.
			return (sizeof($common) == sizeof($proposed_server_array));
		} else {
			return false;
		}
	}
	
	
	/**
	 * Save the current order and timestamps.
	 */
	private function save_file() {
		if (sizeof($this->server_array > 0)) {
			// Serialize and save to file
			file_put_contents($this->server_filename, serialize($this->server_array));
		}
	}		
}
?>