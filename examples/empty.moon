state main () [] {
    let x: i15 = 1234;
    @asm CAE (x);
    @asm XCH ASSRTVAL;
    @asm TC EXIT;
}
