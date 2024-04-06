// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

#include <stdio.h> // system file
#include "square_root.h"
int main()
{
    printf("%f\n", square_root(4.0));
    return 0;
}
