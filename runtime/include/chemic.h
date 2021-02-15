#ifndef CHEMIC_H
#define CHEMIC_H

/* headers */

#include <stddef.h>
#include <inttypes.h>

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

/* constants */

static const Obj OBJ_0 = { tag_int, { .i = 0 } };
static const Obj OBJ_1 = { tag_int, { .i = 1 } };

/* external functions */

extern Obj make_int(int64_t i);
extern Obj make_string(char *data, size_t size);

extern Obj add(Obj a, Obj b);
extern Obj sub(Obj a, Obj b);
extern Obj neg(Obj a);
extern Obj mul(Obj a, Obj b);
extern Obj len(Obj a);

extern void print(Obj a);

#endif /* CHEMIC_H */
