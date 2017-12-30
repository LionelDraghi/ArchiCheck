Download
========

[Here](http://lionel.draghi.free.fr/Archicheck/archicheck) is an exe build on my Debian amd64,
with -O3 option.

After download : `chmod +x archicheck`

Build :
-------

> date -r archicheck --iso-8601=seconds

```
2017-12-30T14:26:49+01:00
```

> archicheck --version

```
0.5.0

```

Dependencies :
--------------

> readelf -d archicheck | grep 'NEEDED'

```
 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [libc.so.6]
```

Tests :
-------

Run 2017-12-30T14:32:18+01:00

- Successful  58
- Failed      0
- Empty       4
