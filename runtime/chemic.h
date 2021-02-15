#include <stdio.h>
#include <stdlib.h>

typedef int64_t i64;
const i64 MIN_I64 = ~9223372036854775807;

void die(char *msg) {
    fprintf(stderr, "fatal error: %s\n", msg);
    exit(1);
}

i64 add(i64 a, i64 b) {
    i64 res;
    if (__builtin_add_overflow(a, b, &res)) {
        die("overflow in + operator");
    }
    return res;
}

i64 sub(i64 a, i64 b) {
    i64 res;
    if (__builtin_sub_overflow(a, b, &res)) {
        die("overflow in - operator");
    }
    return res;
}

i64 mul(i64 a, i64 b) {
    i64 res;
    if (__builtin_mul_overflow(a, b, &res)) {
        die("overflow in * operator");
    }
    return res;
}

i64 neg(i64 a) {
    if (a == MIN_I64) {
        die("underflow in unary - operator");
    }
    return -a;
}

void print(i64 a) {
    printf("%lld\n", a);
}
