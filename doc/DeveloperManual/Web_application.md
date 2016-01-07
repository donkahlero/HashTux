#Web application
##URL Rewrites
Since we want URLs to look good, we use mod_rewrite on Apache. In the .htaccess file, 
we specify that everything following the domain name in the url should be seen as a search query, with a few exceptions.
So if we visit for example
```
http://www.hashtux.com/car 
```
this will correspond to searching for car in the search box. This is rewritten by the mod_rewrite module to 
```
http://www.hashtux.com/search.php?search=car
```
The exceptions include our specific stylesheet, javascript and imagee resource files and folders and whatever other resources we need to make available.
