#ifndef CHEMIC_H
#define CHEMIC_H

/*= headers =*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/*= type definitions =*/

typedef enum { tag_nil, tag_int, tag_proc, tag_str, tag_cons } Tag;

typedef struct {
    size_t len;
    // number of owning references to this string
    // (if zero, this is a static string that should never be freed)
    size_t ref_count;
    uint8_t data[];
} Str;

struct Cons_s;

typedef struct Obj_s {
    Tag tag;
    union {
        int64_t i;
        struct Obj_s (*p)();
        Str *s;
        struct Cons_s *c;
    } data;
} Obj;

typedef struct Cons_s {
    Obj car;
    Obj cdr;
} Cons;

typedef struct {
    Obj *buf;
    size_t len;
    size_t cap;
} ArgVec;

/*= shared global variables =*/

extern ArgVec call_args;

/*= macros/inline functions =*/

inline static char const* classify(Tag t) {
    switch (t) {
        case tag_nil: return "nil";
        case tag_int: return "int";
        case tag_proc: return "procedure";
        case tag_str: return "string";
        case tag_cons: return "cons";
    }
}

#define DIE(MSG) \
    if (1) { \
        fputs("fatal error: " MSG "\n", stderr); \
        exit(1); \
    } else (void) 0

#define FDIE(MSG, ...) \
    if (1) { \
        fprintf(stderr, "fatal error: " MSG "\n", __VA_ARGS__); \
        exit(1); \
    } else (void) 0

// if T is not the expected type, take ownership of T and raise an error
#define EXPECT(T, TAG) \
    if (T.tag != TAG) { \
        deinit(T); \
        FDIE("expected %s, got %s", classify(TAG), classify(T.tag)); \
    } else (void) 0

#define MAKE_INT(T, I) \
    if (1) { \
        T.tag = tag_int; \
        T.data.i = I; \
    } else (void) 0

#define MAKE_STRING(T, S) \
    if (1) { \
        T.tag = tag_str; \
        T.data.s = S; \
    } else (void) 0

#define MAKE_PROC(T, P) \
    if (1) { \
        T.tag = tag_proc; \
        T.data.p = P; \
    } else (void) 0

#define NIL (Obj) {tag_nil}
#define MAKE_NIL(T) T.tag = tag_nil

/* inline */ static void arg_init(size_t n) {
    call_args.len = 0;
    if (n > call_args.cap) {
        // round up to avoid reallocating frequently
        call_args.cap = (n | 7) + 1;
        call_args.buf = realloc(call_args.buf, call_args.cap * sizeof(Obj));
        if (call_args.buf == NULL) {
            DIE("out of memory");
        }
    }
}

inline static void arg_push(Obj a) {
    call_args.buf[call_args.len] = a;
    call_args.len++;
}

/*= external functions =*/

extern void arg_init(size_t n);
extern void finalize();

// take & return ownership of their arguments
extern Obj add(Obj a, Obj b);
extern Obj sub(Obj a, Obj b);
extern Obj neg(Obj a);
extern Obj mul(Obj a, Obj b);
extern Obj len(Obj a);
extern Obj cons(Obj a, Obj b);
extern Obj call(Obj a);
extern void arg_push(Obj a);
extern void deinit(Obj a);
// do not take ownership of their argument
extern void clone(Obj a);
extern void display(Obj a);

#endif /* CHEMIC_H */
