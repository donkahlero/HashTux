#NetBeans Setup for developing Hashtux
## Step-By-Step (One-By-One - Higher and Hiiiiigher)
* Clone this repository in the location of your choice
* Create a new NetBeans project:
* Select PHP -> PHP with existing Sources
* Sources Folder: HashTuxRepo/website
* Project Name: HashTux
<br>
![](https://github.com/TacoVox/HashTux/blob/master/repofiles/step_1.png)
* Run As: Remote WebSite (SFTP, etc.)
* Project URL: http://dev.hashtux.com
<br>
![](https://github.com/TacoVox/HashTux/blob/master/repofiles/step_2.png)
* Remote Connection -> Click on Manage
* Create Connection.
* Name: dev.hashtux.com
* Type: SFTP
* Click Ok
<br>
![](https://github.com/TacoVox/HashTux/blob/master/repofiles/step_3.png)
* Host Name: dev.hashtux.com - Port: 22
* User Name: tux
* Password: grouptux
* Initial Directory: /home/tux/hashtux/dev
* Click ok again :D
<br>
![](https://github.com/TacoVox/HashTux/blob/master/repofiles/step_4.png)
* Upload directory here: /home/tux/hashtux/dev
<br>
![](https://github.com/TacoVox/HashTux/blob/master/repofiles/step_5.png)
* That's it. The project should show up now:
<br>
![](https://github.com/TacoVox/HashTux/blob/master/repofiles/step_6.png)

## Workflow:
* Pull the newest changes
* Work with NetBeans. -> When you safe - the file will be saved on your local disk as well as on the server
* After you are done commit your changes to the development branch on GitHub (whichever it will be).
* As soon as major features are done on the website - merge them to the website release branch. They will be autofetched to our three webservers and will be shown there.
