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

static Obj operator_add() {
    Obj a;
    MAKE_INT(a, 0);
    for (size_t i = 0; i < call_args.len; i++) {
        a = add(a, call_args.buf[i]);
    }
    call_args.len = 0;
    return a;
}

static Obj operator_sub() {
    size_t len = expect_args_min(1);
    Obj a = call_args.buf[0];
    for (size_t i = 1; i < len; i++) {
        a = sub(a, call_args.buf[i]);
    }
    return len == 1 ? neg(a) : a;
}

static Obj operator_mul() {
    Obj a;
    MAKE_INT(a, 1);
    for (size_t i = 0; i < call_args.len; i++) {
        a = mul(a, call_args.buf[i]);
    }
    call_args.len = 0;
    return a;
}

static Obj operator_less_than() {
    expect_args_exact(2);
    return less_than(call_args.buf[0], call_args.buf[1]);
}

static Obj operator_len() {
    expect_args_exact(1);
    return len(call_args.buf[0]);
}

static Obj operator_display() {
    expect_args_exact(1);
    display(call_args.buf[0]);
    return call_args.buf[0];
}

static Obj operator_cons() {
    expect_args_exact(2);
    return cons(call_args.buf[0], call_args.buf[1]);
}

static Obj operator_make_ref() {
    expect_args_exact(1);
    return make_ref(call_args.buf[0]);
}

static Obj operator_deref() {
    expect_args_exact(1);
    return deref(call_args.buf[0]);
}

static Obj operator_counter() {
    expect_args_exact(0);
    return make_counter();
}

static Obj operator_string_copy() {
    expect_args_exact(1);
    return string_copy(call_args.buf[0]);
}

static Obj operator_dbg() {
    expect_args_exact(0);
    gc_debug();
    return NIL;
}

static Obj operator_gc_collect() {
    expect_args_exact(0);
    gc_collect();
    return NIL;
}
