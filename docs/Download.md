Download
========

[Here](http://lionel.draghi.free.fr/Archicheck/archicheck) is an exe build on my Debian amd64,
with -O3 option.

Build
-----

```
date -r archicheck --iso-8601=seconds

2017-12-06T00:07:40+01:00
```

Dependencies:
-------------

```
readelf -d archicheck | grep 'NEEDED'

 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [libopentoken.so.9]
 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [libgcc_s.so.1]
 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [libc.so.6]
```
