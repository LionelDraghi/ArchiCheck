Download
========

[Download Linux exe](http://lionel.draghi.free.fr/Archicheck/archicheck)

build on :
----------

> uname -orm

```
5.19.0-2-amd64 x86_64 GNU/Linux
```

> gnat --version | head -1

```
GNAT 12.2.0
```
and -O3 option.

(May be necessary after download : `chmod +x archicheck`)

Exe check :
-----------

> date -r archicheck --iso-8601=seconds

```
2022-10-05T01:06:57+02:00
```

> readelf -d archicheck | grep 'NEEDED'

```
 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [libc.so.6]
 0x0000000000000001 (NEEDED)             Bibliothèque partagée: [ld-linux-x86-64.so.2]
```

> archicheck --version

```
0.5.8
```

Tests status on this exe :
--------------------------

Run 2022-10-05T01:07:17+02:00

- Successful  82
- Failed      0
- Empty       4
