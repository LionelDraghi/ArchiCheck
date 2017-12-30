
# Non recursive file identification test suite



##  Non recursive file identification test suite / Simple test


  if sources :
  > ./dir3/c-d.ads
  > ./dir3/c.ads
  > ./dir1/a.ads
  > ./dir1/a.adb
  > ./dir2/b.ads
  then
  > archicheck -I dir1 -I dir2 -I dir3 --list_files
  should put:
```
dir1/a.adb
dir1/a.ads
dir2/b.ads
dir3/c.ads
dir3/c-d.ads
```


 Non recursive file identification test suite / Simple test [Successful](tests_status.md#successful)
