#ifndef CHEMIC_H
#define CHEMIC_H

/*= headers =*/

#include <stdio.h>
#include <stdlib.h>

/*= type definitions =*/

typedef enum { tag_nil, tag_int, tag_str } Tag;

typedef struct {
    size_t len;
    // number of owning references to this string
    // (if zero, this is a static string that should never be freed)
    size_t ref_count;
    uint8_t data[];
} Str;

typedef struct {
    Tag tag;
    union {
        int64_t i;
        Str *s;
    } data;
} Obj;

/*= macros/inline functions =*/

inline static char const* classify(Tag t) {
    switch (t) {
        case tag_nil: return "nil";
        case tag_int: return "int";
        case tag_str: return "string";
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

/*= external functions =*/

// take & return ownership of their arguments
extern Obj add(Obj a, Obj b);
extern Obj sub(Obj a, Obj b);
extern Obj neg(Obj a);
extern Obj mul(Obj a, Obj b);
extern Obj len(Obj a);
extern void deinit(Obj a);
// does not take ownership of its argument
extern void print(Obj a);

extern void finalize();

#endif /* CHEMIC_H */
