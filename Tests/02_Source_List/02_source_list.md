# Feature: --list_files option and sources finding feature

## Scenario: Non recursive file identification test

A list of directory to explore is given to acc with the `-I` option.

- Given the `./dir1/` dir
- Given the `./dir2/` dir
- Given the `./dir3/` dir

- Given the file `./dir3/c-d.ads`
```ada
package C.D is
end;
```    
- And   the file `./dir3/c.ads`    
```ada
package C is
end;
```    
- And   the file `./dir1/a.ads`    
```ada
package A is
end;
```    
- And   the file `./dir1/a.adb`    
```ada
package body B is
end;
```    
- And   the file `./dir2/b.ads`    
```ada
package body B is
end;
```    

- When I run `acc -I dir1 -I dir2 -I dir3 --list_files`  

- Then the output is 
```  
dir1/a.adb
dir1/a.ads
dir2/b.ads
dir3/c-d.ads
dir3/c.ads
```  

## Scenario: Recursive file identification test

When used, `-Ir` cause the following directory to be explored recursively.

- Given the `./dira/` dir
- Given the `./dirb/` dir
  
Those first dir creation are only needed because yet bbt is only recording the creation of the leaf (dira1) and not the creation of intermediate dir (dira). As a consequence, the --cleanup let `dira` behind.  
[Issue #3](https://github.com/LionelDraghi/bbt/issues/3)

- Given the `./dira/dira1/` dir
- Given the `./dirb/dirb1/` dir

- Given the file `./dira/a.ads`
```ada
with B;
package A is
end;
```    
- And   the file `./dirb/b.ads`    
```ada
with K;
package B is
end;
```    
- And   the file `./dirb/dirb1/c.ads`    
```ada
with A;
package C is
end;
```    
- And   the file `./dira/dira1/c-d.ads`    
```ada
with B;
package C.D is
end;
```    

- When I run `../../obj/acc -I dira -Ir dirb --list_files`  

- Then the output is 
```  
dira/a.ads
dirb/b.ads
dirb/dirb1/c.ads
```  


