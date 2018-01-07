Download
========

[Download Linux exe](http://lionel.draghi.free.fr/Archicheck/archicheck), build on my Debian amd64,
with -O3 option.

(May be necessary after download : `chmod +x archicheck`)

> date -r archicheck --iso-8601=seconds

```
2018-01-06T00:07:23+01:00
```

> readelf -d archicheck | grep 'NEEDED'

```
 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [libc.so.6]
```

> archicheck --version

```
0.5.4

```

Tests status on this exe :
--------------------------

Run 2018-01-06T00:07:41+01:00

- Successful  68
- Failed      0
- Empty       5
