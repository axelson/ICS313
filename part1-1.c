#include <stdio.h>

int main() {
  int j, i;
  j = -3;
  for (i = 0; i< 3; i++) {
    printf("top: i=%d, j=%d\n", i, j);
    switch (j + 2) {
      case 3:
      case 2: j--; break;
      case 0: j += 2; break;
      default: j = 0;
    }
    printf("mid: i=%d, j=%d\n", i, j);
    if (j > 0) break;
    j = 3 -i;
    printf("bot: i=%d, j=%d\n", i, j);
  }
  return 0;
}
