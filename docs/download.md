Download
========

[Download Linux exe](http://lionel.draghi.free.fr/Archicheck/archicheck)

build on :
----------

> uname -orm

```
4.18.0-2-amd64 x86_64 GNU/Linux
```

> gnat --version | head -1

```
GNAT Community 2018 (20180524-73)
```

(May be necessary after download : `chmod +x archicheck`)

Exe check :
-----------

> date -r archicheck --iso-8601=seconds

```
2018-11-11T05:34:52+01:00
```

> readelf -d archicheck | grep 'NEEDED'

```
 0x0000000000000001 (NEEDED)             Shared library: [libdl.so.2]
 0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]
 0x0000000000000001 (NEEDED)             Shared library: [ld-linux-x86-64.so.2]
```

> archicheck --version

```
0.5.8
```

Tests status on this exe :
--------------------------

Run 2018-11-11T11:20:19+01:00

- Successful  82
- Failed      0
- Empty       4
