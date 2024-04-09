Download
========

[Download Linux exe](http://lionel.draghi.free.fr/Archicheck/archicheck)

build on :
----------

> uname -orm

```
6.6.15-amd64 x86_64 GNU/Linux
```

> gnat --version | head -1

```
GNAT 13.2.0
```
and -O3 option.

(May be necessary after download : `chmod +x archicheck`)

Exe check :
-----------

> date -r archicheck --iso-8601=seconds

```
2024-04-09T14:16:49+02:00
```

> readelf -d archicheck | grep 'NEEDED'

```
 0x0000000000000001 (NEEDED)             Bibliothèque partagée : [libc.so.6]
 0x0000000000000001 (NEEDED)             Bibliothèque partagée : [ld-linux-x86-64.so.2]
```

> archicheck --version

```
0.5.8
```

Tests status on this exe :
--------------------------

Run 2024-04-09T14:16:58+02:00

- Successful  86
- Failed      0
- Empty       4
