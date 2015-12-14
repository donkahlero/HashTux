#How to install and deploy Hashtux

##Web application
We use Apache with a dedicated vhost for the HashTux web application. 
- PHP is required. We recommend not using "strict" error reporting.
- The cURL module for PHP is required.
- mod_rewrite for Apache is needed, and AllowOverride needs to be set to all in the root folder of the web server, so our .htaccess file works properly.
- The folder in "website" should be set as the root folder of the web server.
- Make sure the web server has write access to this folder.
- Copy the config.php.sample config file to config.php (located in in webiste/conf).
- Make all necessary changes to the config file to suit your needs. The proposed settings should work for everything, except the array $config['backend_servers'], which should be changed to contain addresses to all Erlang backend servers, including port numbers (example: "dev.hashtux.com:8080").

##Erlang backend
The Erlang backend requires Erlang 18. Dependencies are handled by Rebar 3. There is a rebar3 binary in the erlang folder, which should work for a general gnu/posix system.
- Configure the backend by copying the sys.config.sample file to sys.config (located in erlang/conf).
- Make all necessary changes to the config file to suit your needs. You can configure the port at which the Erlang backend listens to HTTP requests. You need at least one primary DB, referred to as "localdb" in the config file. If you have no external DBs, provide [] as the value for external. The other settings mostly concern API keys for each social media API that requires it.
- You should then be able to run "./rebar3 shell" in the erlang folder to try the Erlang backend.

##CouchDB
Hashtux recommend installing CouchDB on a Linux or Unixoid operating system.
For the major Linux distros CouchDB is in their repositories. Just install it that way and the dependencies will get fetched as well.
For all other OS without this possibilty check out http://couchdb.apache.org/ and find the fitting installation package.
For the running hashtux three databases are required:
- hashtux
- hashtux_userstats
- hashtux_userstats_chached_data
A user for accessing the database is also required. Password and name is at the admins discretion. Just change the config file accordingly.
