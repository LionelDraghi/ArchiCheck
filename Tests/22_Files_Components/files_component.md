
# Adding compilation units through files to a Component



##  Adding compilation units through files to a Component / Sources identification through rules file (no -I on command line)

  (src not detailed here)  
  > acc -I dir1 --list_dependencies  
  should put:  

```  
A package body depends on C 
A package spec depends on B 
```  


Adding compilation units through files to a Component / Sources identification through rules file (no -I on command line) [Successful](files_component.md#adding-compilation-units-through-files-to-a-component--sources-identification-through-rules-file-no--i-on-command-line)

##  Adding compilation units through files to a Component / Source with weird formatted withed unit

  same test here, but `c-d.adb` if formatted this way :  

```  
with   
     E, -- xyz
  F, G;
function C .D is
   null;
end C . D ;
```  


Adding compilation units through files to a Component / Source with weird formatted withed unit [Successful](files_component.md#adding-compilation-units-through-files-to-a-component--source-with-weird-formatted-withed-unit)
