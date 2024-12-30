#include <stdio.h>
void process(int n) { for (int i1=1; i1<=n; i1++){ int flag = 0;if (!(i1 % 3)) {
flag=1;
int i3= i1/ 3;
fputs("Fizz", stdout);
}
if (!(i1 % 5)) {
flag=1;
int i5= i1/ 5;
fputs("Buzz", stdout);
}
if (!flag) printf("%d", i1);
putchar('\n');}}
int main() {
int n = 0;
scanf("%d", &n); if (n == 0) return 1; process(n); return 0;}
