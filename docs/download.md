Download
========

[Download Linux exe](https://github.com/LionelDraghi/ArchiCheck/releases)

build on :
----------

> uname -orm

```
7.2.9+deb14-amd64 x86_64 GNU/Linux
```

> gnat --version | head -1

```
```
and -O3 option.

(May be necessary after download : `chmod +x acc`)

Exe check :
-----------

> date -r acc --iso-8601=seconds

```
2026-10-10T16:47:35+02:00
```

> readelf -d acc | grep 'NEEDED'

```
 0x0000000000000001 (NEEDED)             Bibliothèque partagée : [libc.so.6]
 0x0000000000000001 (NEEDED)             Bibliothèque partagée : [ld-linux-x86-64.so.2]
```

> acc --version

```
0.6.1
```

Tests status on this exe :
--------------------------

Run 2026-10-10T16:48:17+02:00

- Failed 0
- Successful 103
- Empty 0
- Not Run 1
