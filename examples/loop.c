int x;
int arr[50];

int main() {
    for (x = 0; x < 50; x = x + 1) {
        if (x == 33) {
            break;
        }
        if (x == 20) {
            continue;
        }
        arr[x] = x;
    }
    return 0;
}
