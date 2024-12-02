state main () [] {
    let x: i15 = 1234;
    @asm CAE (x);
    @asm TC EXIT;
}
