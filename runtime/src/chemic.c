#include "chemic.h"

#include <stdio.h>
#include <stdlib.h>

#define I64_MIN (~9223372036854775807)

Obj add(Obj a, Obj b) {
    EXPECT(a, tag_int, "int");
    EXPECT(b, tag_int, "int");
    int64_t res;
    if (__builtin_add_overflow(a.data.i, b.data.i, &res)) {
        DIE("addition overflow");
    }
    MAKE_INT(a, res);
    return a;
}

Obj sub(Obj a, Obj b) {
    EXPECT(a, tag_int, "int");
    EXPECT(b, tag_int, "int");
    int64_t res;
    if (__builtin_sub_overflow(a.data.i, b.data.i, &res)) {
        DIE("subtraction overflow");
    }
    MAKE_INT(a, res);
    return a;
}

Obj neg(Obj a) {
    EXPECT(a, tag_int, "int");
    if (a.data.i == I64_MIN) {
        DIE("negate underflow");
    }
    MAKE_INT(a, -a.data.i);
    return a;
}

Obj mul(Obj a, Obj b) {
    EXPECT(a, tag_int, "int");
    EXPECT(b, tag_int, "int");
    int64_t res;
    if (__builtin_mul_overflow(a.data.i, b.data.i, &res)) {
        DIE("multiplication overflow");
    }
    MAKE_INT(a, res);
    return a;
}

Obj len(Obj a) {
    EXPECT(a, tag_str, "str");
    size_t len = a.data.s->len;
    deinit(a);
    MAKE_INT(a, len);
    return a;
}

inline static Str *str_dup(Str *s) {
    Str *new = malloc(sizeof(Str) + s->len);
    if (!new) { DIE("out of memory"); }
    new->len = s->len;
    memcpy(&new->data, &s->data, s->len);
    return new;
}

static Obj copy(Obj a) {
    switch (a.tag) {
        case tag_int:
            break;
        case tag_str:
            a.data.s = str_dup(a.data.s);
            break;
    }
    return a;
}

static Obj reg = {.tag = tag_int, .data = {.i = 1234}};

void reg_save(Obj a) {
    deinit(reg);
    reg = copy(a);
}

Obj reg_restore() {
    return copy(reg);
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

void deinit(Obj o) {
    if (o.tag == tag_str) {
        free(o.data.s);
    }
}

void finalize() {
    deinit(reg);
}
