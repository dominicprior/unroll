# unroll
A first attempt at unrolling some simple C++ loops using Haskell Parsec.

For example, it can just about translate this:
```
n=3;
for (i=0; i<n; i++) {
  for (j=0; j<=i; j++) {
    f(i,j);
  }
}
```
into this:
```
f(0,0);
f(1,0);
f(1,1);
f(2,0);
f(2,1);
f(2,2);
```
