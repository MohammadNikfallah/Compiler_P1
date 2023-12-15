# Compiler project (Spring 2023)

A simple compiler project based on llvm. You can find the report [here](https://docs.google.com/document/d/e/2PACX-1vTfWtYM_N_mQk5RikDV1RG3H3LZJlTMFJCe3y-PfCH_TrFfJ_gmcrWPYtJ7gA8BhsEbtHEf0-gVhAqZ/pub)

## How to run?
```
mkdir build
cd build
cmake ..
make
cd build
./gsm "<the input you want to be compiled>" > gsm.ll
llc --filetype=obj -o=gsm.o gsm.ll
clang -o gsmbin gsm.o ../../rtGSM.c
```

## Sample inputs
```
type int a;
```

```
type int a = 3 * 9;
```

```
type int a = 4;
type int b = 4 * 9;
type int c;
c = a * b;
```
