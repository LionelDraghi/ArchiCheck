
# File identification test suite



##  File identification test suite / Non recursive file identification test


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


File identification test suite / Non recursive file identification test [Successful](tests_status.md#successful)

##  File identification test suite / Mixt recursive and non-recursive file identification test


  if sources :  
  > ./dira/a.ads    
  > ./dira/dira1/c-d.ads    
  > ./dirb/b.ads    
  > ./dirb/dirb1/c.ads    

  then  

  > archicheck -I dira -Ir dirb --list_files  

  should put:  
```  
dira/a.ads
dirb/b.ads
dirb/dirb1/c.ads
```  


File identification test suite / Mixt recursive and non-recursive file identification test [Successful](tests_status.md#successful)
