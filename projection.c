#include <stdlib.h>

struct rgba {
    char r;
    char g;
    char b;
    char a;
};

struct mapping {
    int source;
    int index;
};

void reproject(struct rgba **src, struct rgba *dest, struct mapping *map, int size) {
    int i;
    for(i = 0; i < size; i++) dest[i] = src[map[i].source][map[i].index];
}

int main(int argc, char **argv) {return 0;}
