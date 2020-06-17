# psql-svg

Basic svg path to [ppm image](https://en.wikipedia.org/wiki/Netpbm#File_formats) renderer written in PostgreSQL.


## Requirements

- A running PostgreSQL server
- ImageMagick (`make demo` command only)


## Usage

Probably don't use this in production.

```shell
make install
make demo
```

Since `psql` is unable to output bytes, images are output base64 encoded.
