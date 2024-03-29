#include "chemic.h"

#include <string.h>
#include <inttypes.h>
#include <stdbool.h>

struct {
    // The half of the heap currently containing live objects
    unsigned char *alive;
    // The half of the heap currently used for scratch space
    unsigned char *dead;
    // The size of the alive allocation - we don't care about the dead one
    size_t cap;
    // How much of the alive allocation is currently in use
    size_t len;
} heap = {
    .alive = NULL,
    .dead = NULL,
    .cap = 0,
    .len = 0,
};

#define INITIAL_HEAP_SIZE 2048

void initialize(void) {
    unsigned char *alive = malloc(INITIAL_HEAP_SIZE);
    if (alive == NULL) {
        DIE("out of memory");
    }
    unsigned char *dead = malloc(INITIAL_HEAP_SIZE);
    if (dead == NULL) {
        DIE("out of memory");
    }
    heap.alive = alive;
    heap.dead = dead;
    heap.cap = INITIAL_HEAP_SIZE;
    heap.len = 0;
}

void finalize(void) {
    free(call_args.buf);
    call_args.len = 0;
    call_args.cap = 0;

    free(heap.alive);
    free(heap.dead);
    heap.len = 0;
    heap.cap = 0;
}

// NOTE: casting the returned pointer violates strict aliasing. Unfortunately
// it seems that there's basically no way to write custom allocators without
// doing so.
static void *heap_alloc(size_t align, size_t len) {
    // TODO: only check this in debug mode
    if (align == 0 || (align & (align - 1)) != 0) {
        DIE("alignment must be power of 2");
    }
    size_t alloc_start = (heap.len + (align - 1)) & ~(align - 1);
    size_t alloc_end = alloc_start + len;
    if (alloc_end < heap.cap) {
        heap.len = alloc_end;
        return heap.alive + alloc_start;
    } else {
        return NULL;
    }
}

// Attempt to allocate space for an object on the heap, but perform a GC
// cycle if there is no space remaining.
static void *retry_heap_alloc(size_t align, size_t len) {
    void *p = heap_alloc(align, len);
    if (!p) {
        gc_collect();
        p = heap_alloc(align, len);
        if (!p) {
            DIE("out of memory");
        }
    }
    return p;
}

// Allocate and return a new vector of the given length,
// with the `len` and `gc_tag` fields correctly set, but
// whose contents are uninitialized. Can trigger GC.
Vect *alloc_vect(size_t len) {
    size_t size = sizeof(Vect) + len * sizeof(Obj);
    Vect *v = retry_heap_alloc(alignof(Vect), size);
    v->gc_tag = NULL;
    v->len = len;
    return v;
}

#define CALL_STACK_MAX_HEIGHT 2048

typedef struct {
    Obj *roots;
    size_t count;
} GcStackFrame;

struct {
    GcStackFrame frames[CALL_STACK_MAX_HEIGHT];
    size_t height;
} gc_stack;

void gc_push_roots(Obj *roots, size_t count) {
    if (gc_stack.height == CALL_STACK_MAX_HEIGHT) {
        DIE("stack overflow");
    }
    gc_stack.frames[gc_stack.height].roots = roots;
    gc_stack.frames[gc_stack.height].count = count;
    gc_stack.height++;
}

void gc_pop_roots(void) {
    gc_stack.height--;
}

void gc_debug(void) {
    printf("\n* current stack height: %zu\n", gc_stack.height);
    printf("* alive heap size: %zu\n", heap.cap);
    printf("* alive heap used: %zu\n", heap.len);
}

static void gc_mark_and_copy_vect(Vect **v);

static void gc_mark_and_copy(Obj *o) {
    switch (o->tag) {
        case tag_nil:
        case tag_true:
        case tag_false:
        case tag_int:
        case tag_proc:
            break;

        case tag_closure:
            {
                Closure *c = o->data.cl;
                if (c->gc_tag == GC_SKIPME) break;

                if (c->gc_tag == NULL) {
                    gc_mark_and_copy_vect(&c->env);
                    Closure *new = heap_alloc(alignof(Closure), sizeof(Closure));
                    if (new == NULL) {
                        DIE("out of memory");
                    }
                    *new = *c;
                    c->gc_tag = new;
                }
                o->data.cl = c->gc_tag;
            }
            break;

        case tag_str:
            {
                Str *s = o->data.s;
                if (s->gc_tag == GC_SKIPME) break;

                if (s->gc_tag == NULL) {
                    size_t size = sizeof(Str) + s->len;
                    Str *new = heap_alloc(alignof(Str), size);
                    if (new == NULL) {
                        DIE("out of memory");
                    }
                    memcpy(new, s, size);
                    s->gc_tag = new;
                }
                o->data.s = s->gc_tag;
            }
            break;

        case tag_cons:
            {
                Cons *c = o->data.c;
                if (c->gc_tag == GC_SKIPME) break;

                if (c->gc_tag == NULL) {
                    gc_mark_and_copy(&c->car);
                    gc_mark_and_copy(&c->cdr);
                    Cons *new = heap_alloc(alignof(Cons), sizeof(Cons));
                    if (new == NULL) {
                        DIE("out of memory");
                    }
                    *new = *c;
                    c->gc_tag = new;
                }
                o->data.c = c->gc_tag;
            }
            break;

        case tag_vect:
            gc_mark_and_copy_vect(&o->data.v);
            break;
    }
}

static void gc_mark_and_copy_vect(Vect **v) {
    if ((*v)->gc_tag == GC_SKIPME) return;

    if ((*v)->gc_tag == NULL) {
        size_t len = (*v)->len;
        for (size_t i = 0; i < len; i++) {
            gc_mark_and_copy(&(*v)->contents[i]);
        }
        size_t size = sizeof(Vect) + len * sizeof(Obj);
        Vect *new = heap_alloc(alignof(Vect), size);
        if (new == NULL) {
            DIE("out of memory");
        }
        memcpy(new, *v, size);
        (*v)->gc_tag = new;
    }
    *v = (*v)->gc_tag;
}

void gc_collect(void) {
    // swap the alive and dead heap pointers
    {
        unsigned char *tmp = heap.alive;
        heap.alive = heap.dead;
        heap.dead = tmp;
    }
    // empty the new heap, because objects are about to be copied to it
    heap.len = 0;
    // mark and copy all objects in the GC stack
    for (size_t i = 0; i < gc_stack.height; i++) {
        GcStackFrame frame = gc_stack.frames[i];
        for (size_t j = 0; j < frame.count; j++) {
            gc_mark_and_copy(&frame.roots[j]);
        }
    }
    // mark and copy all function arguments
    for (size_t i = 0; i < call_args.len; i++) {
        gc_mark_and_copy(&C_ARG(i));
    }
}

#define I64_MIN (~9223372036854775807)

Obj eqv_q(Obj a, Obj b) {
    if (a.tag != b.tag) {
        return FALSE;
    }
    bool res = false;
    switch (a.tag) {
        case tag_nil:
        case tag_false:
        case tag_true:
            res = true;
            break;
        case tag_int:
            res = (a.data.i == b.data.i);
            break;
        case tag_proc:
            res = (a.data.p == b.data.p);
            break;
        case tag_closure:
            res = (a.data.cl == b.data.cl);
            break;
        case tag_str:
            res = (a.data.s == b.data.s);
            break;
        case tag_cons:
            res = (a.data.c == b.data.c);
            break;
        case tag_vect:
            res = (a.data.v == b.data.v);
            break;
    }
    return res ? TRUE : FALSE;
}

Obj less_than(Obj a, Obj b) {
    EXPECT(a, tag_int);
    EXPECT(b, tag_int);
    return a.data.i < b.data.i ? TRUE : FALSE;
}

Obj add(Obj a, Obj b) {
    EXPECT(a, tag_int);
    EXPECT(b, tag_int);
    if (__builtin_add_overflow(a.data.i, b.data.i, &a.data.i)) {
        DIE("addition overflow");
    }
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

Obj string_q(Obj a) {
    return a.tag == tag_str ? TRUE : FALSE;
}

Obj string_length(Obj a) {
    EXPECT(a, tag_str);
    MAKE_INT(a, a.data.s->len);
    return a;
}

Obj string_copy(Obj a) {
    EXPECT(a, tag_str);

    Str *old = a.data.s;
    Str *new = retry_heap_alloc(alignof(Str), sizeof(Str) + old->len);
    memcpy(new, old, sizeof(Str) + old->len);
    new->gc_tag = NULL;

    a.tag = tag_str;
    a.data.s = new;
    return a;
}

Obj cons(Obj a, Obj b) {
    Cons *c = retry_heap_alloc(alignof(Cons), sizeof(Cons));

    c->gc_tag = NULL;
    c->car = a;
    c->cdr = b;
    a.tag = tag_cons;
    a.data.c = c;
    return a;
}

static void display_str(Str *s) {
    fwrite(&s->data, sizeof(uint8_t), s->len, stdout);
}

static void display_cons_items(Cons c) {
    while (1) {
        display(c.car);
        switch (c.cdr.tag) {
            case tag_cons:
                putchar(' ');
                c = *c.cdr.data.c;
                break;
            case tag_nil:
                return;
            default:
                fputs(" . ", stdout);
                display(c.cdr);
                return;
        }
    }
}

void display(Obj a) {
    switch (a.tag) {
        case tag_nil:
            fputs("()", stdout);
            break;
        case tag_true:
            fputs("#t", stdout);
            break;
        case tag_false:
            fputs("#f", stdout);
            break;
        case tag_int:
            printf("%" PRId64, a.data.i);
            break;
        case tag_proc:
            fputs("#<procedure>", stdout);
            break;
        case tag_closure:
            fputs("#<closure>", stdout);
            break;
        case tag_str:
            display_str(a.data.s);
            break;
        case tag_cons:
            putchar('(');
            display_cons_items(*a.data.c);
            putchar(')');
            break;
        case tag_vect:
            fputs("#<vector>", stdout);
            break;
    }
}

Obj make_closure(ClosureFn f, Vect *env) {
    Closure *clo = retry_heap_alloc(alignof(Closure), sizeof(Closure));
    clo->gc_tag = NULL;
    clo->run = f;
    clo->env = env;

    Obj a;
    a.tag = tag_closure;
    a.data.cl = clo;
    return a;
}

ArgVec call_args = { NULL, 0, 0 };

Obj call(Obj a) {
    switch (a.tag) {
        case tag_proc:
            return a.data.p();
        case tag_closure:
            return a.data.cl->run(a.data.cl->env);
        default:
            EXPECT(a, tag_proc);
            return NIL;
    }
}
