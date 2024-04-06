
# C sanity test suite



##  C sanity test suite / .c and .h files list


  Sources :  

```  
main.c
newton_method.c
square_root.c
```  

  Includes :  

```  
newton_method.h
square_root.h
```  

  When :  

  > -lf -I include -I src   

  Expected :  

```  
include/newton_method.h
include/square_root.h
src/main.c
src/newton_method.c
src/square_root.c
```  


C sanity test suite / .c and .h files list [Successful](tests_status.md#successful)

##  C sanity test suite / dependencies list


  obj/*.d files :  

```  
obj/main.o: src/main.c include/square_root.h
include/square_root.h:
obj/newton_method.o: src/newton_method.c include/newton_method.h
include/newton_method.h:
obj/square_root.o: src/square_root.c include/square_root.h \
 include/newton_method.h
include/square_root.h:
include/newton_method.h:
```  

  When :  

  > archicheck -ld -I include -I src   

  Expected :  

```  
main implementation depends on stdio
main implementation depends on square_root
square_root implementation depends on newton_method
```  


C sanity test suite / dependencies list [Successful](tests_status.md#successful)
