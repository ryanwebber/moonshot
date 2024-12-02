prog {
    .verb = 00;
    .noun = 00;
    .entry = main;
}

state main () [] {
    let c: i15 = 2;
    let q: i15 = add(a: 1, b: c);
}

sub add(a: i15, b: i15) {
    let x: i15 = 234;
    let z: i15 = x;
    return 15 + 17;
    return;
}

sub test_inline_asm() {
    @`		# SOME INLINE ASSEMBLY`;
    @`TESTASM1	TC	TESTASM1`;
    @`		CAE	NEWJOB`;
}
