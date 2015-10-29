# HashTux
### Module documentation
* [CouchDB module](https://github.com/TacoVox/HashTux/blob/master/CouchDB.MD)
### SSH/SFTP access to the development server
* Server address: dev.hashtux.com
* User: tux
* Password: grouptux
* Dev folder is here: /home/tux/hashtux/dev
* Run "./pull_and_run_hashtux.sh" to automatically pull the latest Erlang application from GitHub and run it in an Erlang shell configured by Rebar (inside a screen session).
* To watch debug output from the application, type "screen -r" to resume the screen session. To detatch, press Ctrl+A followed by D.

### How to work with your IDE:
* [Setup NetBeans](https://github.com/TacoVox/HashTux/blob/master/NetBeansSetup.MD)

### Rebar3
Rebar3 is provided in the erlang folder.<br />
Run it with "./rebar3" and any arguments.<br />
Examples: <br />
"./rebar3 shell" to launch the shell with the application started.<br />
"./rebar3 upgrade" to fetch dependencies if they have been updated. <br />
