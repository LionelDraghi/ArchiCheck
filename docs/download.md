Download
========

[Here](http://lionel.draghi.free.fr/Archicheck/archicheck) is an exe build on my Debian amd64,
with -O3 option.

Build :
-------

> date -r archicheck --iso-8601=seconds

```
2017-12-26T19:03:47+01:00
```

> archicheck --version

```
0.4.3

```

Dependencies :
--------------

> readelf -d archicheck | grep 'NEEDED'

```
 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [libc.so.6]
```

Tests :
-------

Run 2017-12-26T19:19:22+01:00

- Successful  50
- Failed      7
- Empty       4
