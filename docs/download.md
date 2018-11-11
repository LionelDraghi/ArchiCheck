Download
========

[Download Linux exe](http://lionel.draghi.free.fr/Archicheck/archicheck), build on my Debian amd64,
with -O3 option.

(May be necessary after download : `chmod +x archicheck`)

> date -r archicheck --iso-8601=seconds

```
2018-10-28T19:45:38+01:00
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

Run 2018-10-28T19:45:48+01:00

- Successful  82
- Failed      0
- Empty       4
