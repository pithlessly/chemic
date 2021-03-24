static Obj operator_add() {
    Obj a;
    MAKE_INT(a, 0);
    for (size_t i = 0; i < call_args.len; i++) {
        a = add(a, call_args.buf[i]);
    }
    call_args.len = 0;
    return a;
}
