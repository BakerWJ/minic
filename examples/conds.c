#include "minicio.h"

bool isEven(int a) {
    if (a == 0)
        return true;
    if (a < 0)
        return false;
    return isEven(a - 2);
}

int main() {
    int input;
    input = getint();

    if (isEven(input)) {
        putint(0);
        return 0;
    }
    putint(1);
    return 1;
}
