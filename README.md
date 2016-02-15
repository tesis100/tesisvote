#Voting Application

###Prerequisits

- GNAT Compiler (tested GNAT-GPL-2015)
- GNATCOLL (tested GPL-2015)
- GNAT Ada Web Server
- NGINX (>=v1.3)

- It assumes to find `gnatcoll_sqlite.gpr` is in `/usr/lib/gnat/gnatcoll_sqlite.gpr`. Change in `voting.gpr`
- Assumes `nginx` is in `/usr/local/nginx/sbin/nginx`. Change in `nginx/makefile` and change `include /usr/local/nginx/conf/mime.types;` in `nginx/nginx.conf`

###Setting Up
`make` will compile, and setup the environment into a state where you can test it. `make run` will launch a webserver at port 443 (may require sudo). If starting fails launching nginx, ensure that port 443 is free and check `nginx/nginx_launch.log`  
Log for operation of server can be `tail -f`ed with make tail or found at `execute/log/voting.log`

Data for setting up is: 
####Codes
List of codes to accept. These are associated with the vote so that cotes can be checked afterwards, and they are one use only. 

####Positions
Valid Syntax: `positionshortname,Position Name,1`
Example: `P,President,1`. `P,President` is also valid, and will assume the `,1`. 
`P` is the shortname used throughout the program. The long name will turn up in the voting interface. And the number at the end says how many vacancies there are for this role. 

####Candidates
Valid Syntax: `This is a name,thisisanamewithoutspaces,Position;Position2;Position3`
Example: `Donald Trump,trump,P;VP` - Donald Trump is running for President and Vice-President. The output ballot will call them 'trump', and the website interface will look for the image trump.jpg. 

####website/intro.text
This determines the text found at the top of the voting interface. 

####website/images
The voting interface will search this folder for images of the candidates. For a good size, 100px by 100px would work well and should be consistent across all pictures. It searches the picture by `candidateshortname.jpg`, i.e. `trump.jpg`