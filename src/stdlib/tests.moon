sub exit_with (value: WORD) {
    @asm CAE (value);
    @asm XCH ASSRTVAL;
    @asm TC EXIT;
}
