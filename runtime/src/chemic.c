#include "chemic.h"

#include <inttypes.h>

#define I64_MIN (~9223372036854775807)

Obj add(Obj a, Obj b) {
    EXPECT(a, tag_int);
    EXPECT(b, tag_int);
    if (__builtin_add_overflow(a.data.i, b.data.i, &a.data.i)) {
        DIE("addition overflow");
    }
    return a;
}

Obj sub(Obj a, Obj b) {
    EXPECT(a, tag_int);
    EXPECT(b, tag_int);
    if (__builtin_sub_overflow(a.data.i, b.data.i, &a.data.i)) {
        DIE("subtraction overflow");
    }
    return a;
}

Obj neg(Obj a) {
    EXPECT(a, tag_int);
    if (a.data.i == I64_MIN) {
        DIE("negate underflow");
    }
    a.data.i = -a.data.i;
    return a;
}

Obj mul(Obj a, Obj b) {
    EXPECT(a, tag_int);
    EXPECT(b, tag_int);
    if (__builtin_mul_overflow(a.data.i, b.data.i, &a.data.i)) {
        DIE("multiplication overflow");
    }
    return a;
}

Obj len(Obj a) {
    EXPECT(a, tag_str);
    size_t len = a.data.s->len;
    deinit(a);
    MAKE_INT(a, len);
    return a;
}

Obj cons(Obj a, Obj b) {
    Cons *c = malloc(sizeof(Cons));
    c->car = a;
    c->cdr = b;
    a.tag = tag_cons;
    a.data.c = c;
    return a;
}

inline static void str_del(Str *s) {
    if (s->ref_count > 0) {
        s->ref_count--;
        if (s->ref_count == 0) {
            free(s);
        }
    }
}

void clone(Obj a) {
    switch (a.tag) {
        case tag_nil:
        case tag_int:
        case tag_cons:
            break;
        case tag_str:
            if (a.data.s->ref_count > 0) {
                a.data.s->ref_count++;
            }
            break;
    }
}

static void display_cons_items(Cons c) {
    display(c.car);
    switch (c.cdr.tag) {
        case tag_cons:
            putchar(' ');
            display_cons_items(*c.cdr.data.c);
            break;
        case tag_nil:
            break;
        default:
            fputs(" . ", stdout);
            display(c.cdr);
            break;
    }
}

void display(Obj a) {
    switch (a.tag) {
        case tag_nil:
            fputs("()", stdout);
            break;
        case tag_int:
            printf("%" PRId64, a.data.i);
            break;
        case tag_str:
            fwrite(&a.data.s->data, sizeof(uint8_t), a.data.s->len, stdout);
            break;
        case tag_cons:
            putchar('(');
            display_cons_items(*a.data.c);
            putchar(')');
            break;
    }
}

void deinit(Obj o) {
    switch (o.tag) {
        case tag_nil:
        case tag_int:
            break;
        case tag_str:
            str_del(o.data.s);
            break;
        case tag_cons:
            /* TODO: garbage collection */
            break;
    }
}

void finalize() {}
