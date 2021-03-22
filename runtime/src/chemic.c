#include "chemic.h"

#include <inttypes.h>
#include <stdalign.h>

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

void initialize() {
    unsigned char *alive = malloc(INITIAL_HEAP_SIZE);
    if (alive == NULL) {
        DIE("out of memory");
    }
    heap.alive = alive;
    heap.dead = NULL;
    heap.cap = INITIAL_HEAP_SIZE;
    heap.len = 0;
}

// NOTE: casting the returned pointer violates strict aliasing. Unfortunately
// it seems that there's basically no way to write custom allocators without
// doing so.
void *heap_alloc(size_t align, size_t len) {
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

#define CALL_STACK_MAX_HEIGHT 2048

struct {
    struct {
        Obj *roots;
        size_t count;
    } frames[CALL_STACK_MAX_HEIGHT];
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

void gc_pop_roots() {
    gc_stack.height--;
}

void gc_debug() {
    printf("\n* current stack height: %zu\n", gc_stack.height);
    printf("* alive heap size: %zu\n", heap.cap);
    printf("* alive heap used: %zu\n", heap.len);
}

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

Obj less_than(Obj a, Obj b) {
    EXPECT(a, tag_int);
    EXPECT(b, tag_int);
    // TODO: return true and false rather than 1 and nil
    if (a.data.i < b.data.i) {
        MAKE_INT(a, 1);
    } else {
        MAKE_NIL(a);
    }
    return a;
}

Obj len(Obj a) {
    EXPECT(a, tag_str);
    size_t len = a.data.s->len;
    MAKE_INT(a, len);
    return a;
}

Obj cons(Obj a, Obj b) {
    Cons *c = heap_alloc(alignof(Cons), sizeof(Cons));
    if (!c) {
        DIE("out of memory");
    }

    c->car = a;
    c->cdr = b;
    a.tag = tag_cons;
    a.data.c = c;
    return a;
}

ArgVec call_args = { NULL, 0, 0 };

Obj call(Obj a) {
    EXPECT(a, tag_proc);
    return a.data.p();
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
        case tag_int:
            printf("%" PRId64, a.data.i);
            break;
        case tag_proc:
            fputs("#<procedure>", stdout);
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

void finalize() {
    free(call_args.buf);
}
