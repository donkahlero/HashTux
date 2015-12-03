<?php

/*
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

// This file uses an external library to parse the user agent string into subparts.
require_once ("lib/UserAgentParser.php");
require_once ("conf/config.php");


/**
 * This class maintains an ordered list of backend servers. When we first ask for the
 * preferred server, it gives us the topmost entry in the backend_servers array in the
 * config. However, if requests to this server ever times out or has problems, the using
 * code can call preferred_server_down() to tell this module to adjust the order of the
 * list of backend servers. The list will be stored to file between executions of this
 * script.
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
		
		// Read the file into array if posisble. It is already ordered.
		$this->server_array = $this->load_file();

		// If there was trouble with the file, load_file has returned false.
		if (!$this->server_array) {			
			$this->server_array = [];
			
			// Let's create the key-value array based on the configuration
			foreach ($config['backend_servers'] as $currentserver) {
				// Add each server with a known latest downtime of 0
				$this->server_array[$currentserver] = 0;
			}
			// ...and save it
			$this->save_file();			
		}
	}

	
	/**
	 * Return the preferred server, if the list contains at least one entry
	 * and the using code hasn't gone through all the available servers.
	 */
	public function get_preferred_server() {
		global $config;
		
		$preferred_server = false;
		if (sizeof($this->server_array) > 0 && 
				$this->servers_tried <= sizeof($config['backend_servers'])) {
			$preferred_server = $this->get_topmost_address();
		}
		
		return $preferred_server;
	}
	
	
	/**
	 * Call this if there are problems with the preferred server.
	 */
	public function preferred_server_down() {
		// Keep track of how many servers we've tried so far (during the 
		// lifetime of this object)
		$this->servers_tried++;	

		// Put the current timestamp as the value for the current server
		$this->server_array[$this->get_topmost_address()] = time();
		
		// Sort the array in ascending order - servers with 0 known instances of
		// being down or downtime long ago (likely up again) will be further up
		asort($this->server_array);

		// Write to the servers.order file
		$this->save_file();	
	}
	
	
	/**
	 * Get the adddress string at the top of the array
	 */
	private function get_topmost_address() {		
		$topmost_address = "";

		if (sizeof($this->server_array) > 0 && is_array($this->server_array)) {
			// "Reset" the array to ensure we get the first entry 
			reset($this->server_array);			
			// Then take the first key of the array
			$topmost_address = key($this->server_array);
		}

		return $topmost_address;		
	}
	
	
	/**
	 * Read from the servers.order file and see if it exists and contains all
	 * the servers. If so, return the key-value array. If not, return false.
	 */
	private function load_file() {
		$result = false;
		if (file_exists($this->server_filename)) {
			// Read the file where the array is stored in it's serialized form.
			// Unserialize to get it as an array again 
			$array_result = unserialize(file_get_contents($this->server_filename));
			
			// Only return the loaded array if all entries (keys) match the config
			if ($this->has_configured_servers($array_result)) {
				$result = $array_result;				
			}
		}
		return $result;
	}
	
	
	/**
	 * Make sure all servers from the config (and ONLY those) is present in
	 * the array given as the argument 
	 */
	private function has_configured_servers($proposed_server_array) {
		global $config;
		
		if (is_array($proposed_server_array)) {
			// The server addresses are keys in the server array		
			$proposed_servers = array_keys($proposed_server_array);
			
			// See which values in $proposed_servers are also in the config.
			// In $common, they will have the same keys as in $proposed_servers.
			$common = array_intersect($proposed_servers, $config['backend_servers']);
			
			// Check equality between arrays. === returns true if the arrays
			// have the same key-value pairs. We also need to check that there aren't
			// MORE servers in the config, that would also mean the file isn't accurate.
			return ($common === $proposed_servers && 
					sizeof($common) == sizeof($config['backend_servers']));
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
