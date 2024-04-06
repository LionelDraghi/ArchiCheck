// simple example excerpt from this discussion : https://stackoverflow.com/questions/68766826/how-to-find-the-dependencies-of-a-source-code 

#include "square_root.h"
#include "newton_method.h"
float square_root(float x)
{
    return newton_method(x);
}