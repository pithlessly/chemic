inline static void expect_args_exact(size_t n) {
    if (call_args.len != n) {
        FDIE("wrong number of arguments: procedure expected %zu, got %zu", \
                n, call_args.len);
    }
}

inline static size_t expect_args_min(size_t n) {
    size_t len = call_args.len;
    call_args.len = 0;
    if (len < n) {
        FDIE("wrong number of arguments: procedure expected at least %zu, got %zu", \
                n, call_args.len);
    }
    return len;
}

static Obj operator_add(void) {
    Obj a;
    MAKE_INT(a, 0);
    for (size_t i = 0; i < call_args.len; i++) {
        a = add(a, call_args.buf[i]);
    }
    call_args.len = 0;
    return a;
}

static Obj operator_sub(void) {
    size_t len = expect_args_min(1);
    Obj a = call_args.buf[0];
    for (size_t i = 1; i < len; i++) {
        a = sub(a, call_args.buf[i]);
    }
    return len == 1 ? neg(a) : a;
}

static Obj operator_mul(void) {
    Obj a;
    MAKE_INT(a, 1);
    for (size_t i = 0; i < call_args.len; i++) {
        a = mul(a, call_args.buf[i]);
    }
    call_args.len = 0;
    return a;
}

#define MAKE_UNARY_OP1(NAME, F) \
    static Obj operator_##NAME (void) { \
        expect_args_exact(1); \
        return F(call_args.buf[0]); \
    }

#define MAKE_UNARY_OP2(NAME, F1, F2) \
    static Obj operator_##NAME (void) { \
        expect_args_exact(1); \
        return F1(F2(call_args.buf[0])); \
    }

#define MAKE_UNARY_OP3(NAME, F1, F2, F3) \
    static Obj operator_##NAME (void) { \
        expect_args_exact(1); \
        return F1(F2(F3(call_args.buf[0]))); \
    }

#define MAKE_UNARY_OP4(NAME, F1, F2, F3, F4) \
    static Obj operator_##NAME (void) { \
        expect_args_exact(1); \
        return F1(F2(F3(F4(call_args.buf[0])))); \
    }

#define MAKE_BINARY_OP1(NAME, F) \
    static Obj operator_##NAME (void) { \
        expect_args_exact(2); \
        return F(call_args.buf[0], call_args.buf[1]); \
    }

MAKE_BINARY_OP1(less_than, less_than);

MAKE_UNARY_OP1(len, len);

static Obj operator_display(void) {
    expect_args_exact(1);
    display(call_args.buf[0]);
    return call_args.buf[0];
}

MAKE_BINARY_OP1(cons, cons);

MAKE_UNARY_OP1(car, car);
MAKE_UNARY_OP1(cdr, cdr);

MAKE_UNARY_OP2(caar, car, car);
MAKE_UNARY_OP2(cadr, car, cdr);
MAKE_UNARY_OP2(cdar, cdr, car);
MAKE_UNARY_OP2(cddr, cdr, cdr);

MAKE_UNARY_OP3(caaar, car, car, car);
MAKE_UNARY_OP3(caadr, car, car, cdr);
MAKE_UNARY_OP3(cadar, car, cdr, car);
MAKE_UNARY_OP3(caddr, car, cdr, cdr);
MAKE_UNARY_OP3(cdaar, cdr, car, car);
MAKE_UNARY_OP3(cdadr, cdr, car, cdr);
MAKE_UNARY_OP3(cddar, cdr, cdr, car);
MAKE_UNARY_OP3(cdddr, cdr, cdr, cdr);

MAKE_UNARY_OP4(caaaar, car, car, car, car);
MAKE_UNARY_OP4(caaadr, car, car, car, cdr);
MAKE_UNARY_OP4(caadar, car, car, cdr, car);
MAKE_UNARY_OP4(caaddr, car, car, cdr, cdr);
MAKE_UNARY_OP4(cadaar, car, cdr, car, car);
MAKE_UNARY_OP4(cadadr, car, cdr, car, cdr);
MAKE_UNARY_OP4(caddar, car, cdr, cdr, car);
MAKE_UNARY_OP4(cadddr, car, cdr, cdr, cdr);
MAKE_UNARY_OP4(cdaaar, cdr, car, car, car);
MAKE_UNARY_OP4(cdaadr, cdr, car, car, cdr);
MAKE_UNARY_OP4(cdadar, cdr, car, cdr, car);
MAKE_UNARY_OP4(cdaddr, cdr, car, cdr, cdr);
MAKE_UNARY_OP4(cddaar, cdr, cdr, car, car);
MAKE_UNARY_OP4(cddadr, cdr, cdr, car, cdr);
MAKE_UNARY_OP4(cdddar, cdr, cdr, cdr, car);
MAKE_UNARY_OP4(cddddr, cdr, cdr, cdr, cdr);

static Obj operator_counter(void) {
    expect_args_exact(0);
    return make_counter();
}

MAKE_UNARY_OP1(string_copy, string_copy);

static Obj operator_dbg(void) {
    expect_args_exact(0);
    gc_debug();
    return NIL;
}

static Obj operator_gc_collect(void) {
    expect_args_exact(0);
    gc_collect();
    return NIL;
}
