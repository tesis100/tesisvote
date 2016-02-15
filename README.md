#Voting Application

###Prerequisites

- GNAT Compiler (tested GNAT-GPL-2015)
- GNATCOLL (tested GPL-2015)
- GNAT Ada Web Server
- NGINX (>=v1.3)

- Assumes `gnatcoll_sqlite.gpr` can be found in `/usr/lib/gnat/gnatcoll_sqlite.gpr`. This can be changed in `voting.gpr`.
- Assumes `nginx` is in `/usr/local/nginx/sbin/nginx`. Change in `nginx/makefile` and change `include /usr/local/nginx/conf/mime.types;` in `nginx/nginx.conf`

###Setting Up
Running `make` will compile and set up the environment into a state where you can test it. `make run` will launch a webserver on port 443 (this may require sudo). If starting fails to launch nginx, ensure that port 443 is free and check `nginx/nginx_launch.log`. The operation log of the server can be `tail -f`ed with `make tail` or found at `execute/log/voting.log`.

To set up a vote, the following data must be provided: 
####Codes
List of codes to accept. These are associated with the vote so that cotes can be checked afterwards, and they are one use only. 

####Positions
Valid Syntax: `positionshortname,Position Name,number vacancies`

Example: `P,President,1`. `P,President` is also valid, and will assume the `,1`. 

`P` is the short name used throughout the program. The long name will turn up in the voting interface. The number at the end says how many vacancies there are for this role. 

####Candidates
Valid Syntax: `This is a name,thisisanamewithoutspaces,Position;Position2;Position3`

Example: `Donald Trump,trump,P;VP` - Donald Trump is running for President and Vice-President. The output ballot will call them 'trump', and the website interface will look for the image trump.jpg. 

####website/intro.text
This determines the text found at the top of the voting interface. 

####website/images
The voting interface will search this folder for images of the candidates. Each candidate shoud have their own picture as `candidateshortname.jpg` (eg `trump.jpg`). All pictures should be the same size and 100px by 100px works well.