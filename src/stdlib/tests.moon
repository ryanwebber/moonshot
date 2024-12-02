sub exit_with (value: i15) {
    @asm CAE (value);
    @asm XCH ASSRTVAL;
    @asm TC EXIT;
}
