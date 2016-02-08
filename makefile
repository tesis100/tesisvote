all: build

build: build-dirs createdb
	gprbuild -d -Pvoting.gpr -XLIBRARY_TYPE=static -XXMLADA_BUILD=static -XAWS_BUILD=default
	cd execute && make newdb

build-dirs:
	mkdir -p objects
	mkdir -p execute/log
	mkdir -p db_backups

createdb:
	cd execute && make codes.db

build-nginx:
	cd nginx && make ssl-cert.key

clean:
	rm -f sources_db/*.adb sources_db/*.ads
	rm -f objects/*
	rm -f execute/voting execute/loader execute/export_blt

clean-all: clean
	rm -f execute/codes.db*
	rm -rf execute/log
	cd nginx && make clean
	rm -f votes/*

sq3:
	sqlite3 execute/codes.db

run:
	cd nginx && make start
	cd execute && ./voting ; true
	cd nginx && make stop

tail:
	tail -f execute/log/voting.log

export:
	cd execute && ./export_blt
