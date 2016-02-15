codes.db: ../sources_db/model
	rm -f codes.db
	cd ../sources_db && gnatcoll_db2ada -dbmodel model -output . -dbname ../execute/codes.db -createdb -dbtype sqlite -api codes_db

newdb:
	rm -f codes.db
	cd ../sources_db && gnatcoll_db2ada -dbmodel model -output . -dbname ../execute/codes.db -createdb -dbtype sqlite -api codes_db
	./loader
