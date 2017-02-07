#include <stdlib.h>
#include <stdio.h>
#include <mpc.h>

int main(int argc, char **argv, char **envp)
{
    while(*envp)
        printf("%s\n",*envp++);
}
