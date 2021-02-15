#include "chemic.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define I64_MIN (~9223372036854775807)

inline static void die(char *msg) {
    fprintf(stderr, "fatal error: %s\n", msg);
    exit(1);
}

inline static const char* type_name(Tag t) {
    switch (t) {
        case tag_int: return "int";
        case tag_str: return "string";
    }
}

inline static void kill(Obj o) {
    if (o.tag == tag_str) {
        free(o.data.s);
    }
}

inline static void expect(Obj o, Tag t) {
    if (o.tag != t) {
        kill(o);
        char msg[30];
        sprintf(msg, "expected %s, got %s", type_name(t), type_name(o.tag));
        die(msg);
    }
}

Obj make_int(int64_t i) {
    Obj res;
    res.tag = tag_int;
    res.data.i = i;
    return res;
}

/* allocate an object with the given string value */
Obj make_string(char *data, size_t size) {
    Obj res;
    res.tag = tag_str;
    res.data.s = malloc(sizeof(Str) + size);
    if (!res.data.s) {
        die("cannot allocate memory for string literal");
    }
    res.data.s->len = size;
    memcpy(&res.data.s->data, data, size);
    return res;
}

Obj add(Obj a, Obj b) {
    expect(a, tag_int);
    expect(b, tag_int);
    int64_t res;
    if (__builtin_add_overflow(a.data.i, b.data.i, &res)) {
        die("addition overflow");
    }
    return make_int(res);
}

Obj sub(Obj a, Obj b) {
    expect(a, tag_int);
    expect(b, tag_int);
    int64_t res;
    if (__builtin_sub_overflow(a.data.i, b.data.i, &res)) {
        die("subtraction overflow");
    }
    return make_int(res);
}

Obj neg(Obj a) {
    expect(a, tag_int);
    if (a.data.i == I64_MIN) {
        die("negate underflow");
    }
    return make_int(-a.data.i);
}

Obj mul(Obj a, Obj b) {
    expect(a, tag_int);
    expect(b, tag_int);
    int64_t res;
    if (__builtin_mul_overflow(a.data.i, b.data.i, &res)) {
        die("multiplication overflow");
    }
    return make_int(res);
}

Obj len(Obj a) {
    expect(a, tag_str);
    size_t len = a.data.s->len;
    kill(a);
    return make_int(len);
}

void print(Obj a) {
    switch (a.tag) {
        case tag_int:
            printf("%" PRId64 "\n", a.data.i);
            break;
        case tag_str:
            fwrite(&a.data.s->data, sizeof(uint8_t), a.data.s->len, stdout);
            putchar('\n');
            break;
    }
}
