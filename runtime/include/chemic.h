#ifndef CHEMIC_H
#define CHEMIC_H

/* headers */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <inttypes.h>
#include <string.h>

/* type definitions */

typedef enum { tag_int, tag_str } Tag;

typedef struct {
    size_t len;
    uint8_t data[];
} Str;

typedef struct {
    Tag tag;
    union {
        int64_t i;
        Str *s;
    } data;
} Obj;

/* macro functions */

#define DIE(msg) \
    if (1) { \
        fprintf(stderr, "fatal error: " msg "\n"); \
        exit(1); \
    } else (void) 0

#define EXPECT(T, Tag, Name) \
    if (T.tag != Tag) { \
        kill(T); \
        DIE("expected " Name); \
    } else (void) 0

#define MAKE_INT(T, I) \
    if (1) { \
        T.tag = tag_int; \
        T.data.i = I; \
    } else (void) 0

#define MAKE_STRING(T, S, Len) \
    if (1) { \
        T.tag = tag_str; \
        T.data.s = malloc(sizeof(Str) + Len); \
        if (!T.data.s) { DIE("out of memory"); } \
        T.data.s->len = Len; \
        memcpy(&T.data.s->data, S, Len); \
    } else (void) 0

/* external functions */

extern Obj add(Obj a, Obj b);
extern Obj sub(Obj a, Obj b);
extern Obj neg(Obj a);
extern Obj mul(Obj a, Obj b);
extern Obj len(Obj a);

extern void reg_save(Obj a);
extern Obj reg_restore();

extern void print(Obj a);
extern void kill(Obj a);

extern void finalize();

#endif /* CHEMIC_H */
