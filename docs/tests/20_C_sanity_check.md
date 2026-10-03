## Feature: C sanity test suite

### Background:

- Given there is no `src` directory
- Given there is no `include` directory

### Scenario: .c and .h files list

- Given the directory `include`
- Given the file `include/newton_method.h`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

float newton_method(float x);
```
- Given the file `include/square_root.h`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

float square_root(float x);
```
- Given the directory `src`
- Given the file `src/main.c`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

#include <stdio.h> // system file
#include "square_root.h"
int main()
{
    printf("%f\n", square_root(4.0));
    return 0;
}
```
- Given the file `src/newton_method.c`
```c
#include "newton_method.h"

// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

float newton_method(float x)
{
    return x;
}
```
- Given the file `src/square_root.c`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

#include "square_root.h"
#include "newton_method.h"
float square_root(float x)
{
    return newton_method(x);
}
```
- When I run `./acc -lf -I include -I src`
- Then output is
```
include/newton_method.h
include/square_root.h
src/main.c
src/newton_method.c
src/square_root.c
```

### Scenario: dependencies list

- Given the directory `include`
- Given the file `include/newton_method.h`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

float newton_method(float x);
```
- Given the file `include/square_root.h`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

float square_root(float x);
```
- Given the directory `src`
- Given the file `src/main.c`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

#include <stdio.h> // system file
#include "square_root.h"
int main()
{
    printf("%f\n", square_root(4.0));
    return 0;
}
```
- Given the file `src/newton_method.c`
```c
#include "newton_method.h"

// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

float newton_method(float x)
{
    return x;
}
```
- Given the file `src/square_root.c`
```c
// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

#include "square_root.h"
#include "newton_method.h"
float square_root(float x)
{
    return newton_method(x);
}
```
- When I run `./acc -ld -I include -I src`
- Then output is (unordered)
```
main implementation depends on square_root
main implementation depends on stdio
square_root implementation depends on newton_method
```
