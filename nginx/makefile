nginx=/usr/local/nginx/sbin/nginx

start: ssl-cert.key
	if [ -e "nginx.pid" ]; then make stop; fi
	date >> nginx_launch.log
	sudo $(nginx) -p `pwd`/ -c nginx.conf >> nginx_launch.log 2>&1

stop:
	sudo $(nginx) -p `pwd`/ -c nginx.conf -s stop >> nginx_launch.log 2>&1

ssl-cert.key:
	openssl genrsa -des3 -out ssl-cert.key -passout pass:hello 1024 > /dev/null 2>&1
	yes "" | openssl req -new -key ssl-cert.key -out ssl-cert.csr -passin pass:hello > /dev/null 2>&1
	openssl rsa -in ssl-cert.key -out ssl-cert.key -passin pass:hello > /dev/null 2>&1
	openssl x509 -req -days 365 -in ssl-cert.csr -signkey ssl-cert.key -out ssl-cert.crt > /dev/null 2>&1

clean:
	rm -f ssl-cert* nginx_launch.log access.log error.log
	rm -rf *temp
