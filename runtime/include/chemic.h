#ifndef CHEMIC_H
#define CHEMIC_H

/*= headers =*/

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/*= type definitions =*/

typedef enum {
    tag_nil,
    tag_true,
    tag_false,
    tag_int,
    tag_proc,
    tag_closure,
    tag_str,
    tag_heap_str,
    tag_cons,
    tag_cell,
} Tag;

typedef struct {
    size_t len;
    uint8_t data[];
} Str;

typedef struct HeapStr_s HeapStr;
typedef struct Cons_s Cons;
typedef struct Cell_s Cell;
typedef struct Closure_s Closure;

typedef struct Obj_s {
    Tag tag;
    union {
        int64_t i;
        struct Obj_s (*p)();
        Closure *cl;
        Str *s;
        HeapStr *hs;
        Cons *c;
        Cell *ce;
    } data;
} Obj;

struct HeapStr_s {
    HeapStr *gc_tag;
    Str s;
};

struct Cons_s {
    Cons *gc_tag;
    Obj car;
    Obj cdr;
};

struct Cell_s {
    Cell *gc_tag;
    Obj contents;
};

struct Closure_s {
    Closure *gc_tag;
    Obj (*run)(Cell**);
    size_t env_len;
    Cell *env[];
};

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
        case tag_nil:      return "nil";
        case tag_true:
        case tag_false:    return "boolean";
        case tag_int:      return "int";
        case tag_proc:     return "procedure";
        case tag_closure:  return "closure"; // TODO merge with procedure
        case tag_str:
        case tag_heap_str: return "string";
        case tag_cons:     return "cons";
        case tag_cell:     return "cell";
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

#define MAKE_NIL(T) ((T).tag = tag_nil)
#define IS_NIL(T) ((T).tag == tag_nil)
#define IS_TRUTHY(T) ((T).tag != tag_false)

#define NIL (Obj) {tag_nil}
#define TRUE (Obj) {tag_true}
#define FALSE (Obj) {tag_false}

#define BOX_CONTENTS(A) ((A).data.ce->contents)

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

// This macro is unsafe because it also declares a local variable
// as a counter in the scope where it is used
#define UNSAFE_EXPECT_ARGS(N) \
    if (call_args.len != N) { \
        FDIE("wrong number of arguments: procedure expected %d, got %zu", \
                N, call_args.len); \
    } \
    call_args.len = 0; \
    size_t arg_i = 0;

// This macro is unsafe because it uses the variable defined in the above macro
#define UNSAFE_NEXT_ARG \
    call_args.buf[arg_i++]

/*= external functions =*/

extern Obj add(Obj a, Obj b);
extern Obj sub(Obj a, Obj b);
extern Obj neg(Obj a);
extern Obj mul(Obj a, Obj b);
extern Obj less_than(Obj a, Obj b);
extern Obj len(Obj a);
extern Obj string_copy(Obj a);
extern Obj cons(Obj a, Obj b);
extern Obj make_ref(Obj a);
extern Obj deref(Obj a);
extern Obj make_counter();
extern Obj call(Obj a);
extern void display(Obj a);

extern void gc_push_roots(Obj *roots, size_t count);
extern void gc_pop_roots();
extern void gc_debug();
extern void gc_collect();

extern void initialize();
extern void finalize();

#endif /* CHEMIC_H */
