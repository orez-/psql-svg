.PHONY: clean install reinstall demo

clean:
	psql -c 'DROP SCHEMA IF EXISTS ppm CASCADE;'

install:
	psql -f render.sql

reinstall: clean install

demo:
	@psql -tAqf demo.sql | base64 -d | display
