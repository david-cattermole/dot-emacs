#include <stdio.h>
#include <unistd.h>

int main() {
    printf("Test attach program started with PID: %d\n", getpid());
    printf("Waiting for debugger to attach...\n");

    int counter = 0;
    while (1) {
        printf("Running... Counter: %d\n", counter++);
        sleep(3);

        if (counter > 100) {
            printf("Exiting after 100 iterations\n");
            break;
        }
    }

    return 0;
}
