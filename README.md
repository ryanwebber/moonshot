# Moonshot Language + Toolchain

A language and compiler toolchain targeting the Apollo Guidance Computer. This is a hobby
project started to learn more about the AGC architecture and software.

## Current Status

 * Working: Basic langage parsing, compilation, and code generation
 * Working: Basic function calls working
 * Working: Basic integration tests with `yaAGC`
 * Missing: Most program validation
 * Missing: Most standard library functions
 * Missing: Runtime support for states and program dispatch

## Example

```
// main.mns
inc "progs/write_mem.mns" as write_mem
inc "progs/test_multistate.mns" as test_multistate

prog {
    .verb = 99;
    .noun = 98;
    .entry = write_mem::main;
}

prog {
    .verb = 10;
    .noun = 97;
    .entry = test_multistate::main;
}

// progs/write_mem.mns
state main () [
    addr: i15 = 0;
    value: i15 = 0;
] {
    addr = $dsky::read_value();
    value = $dsky::read_value();
    $mem::write(addr: addr, value: value);
}

// progs/test_multistate.mns
state main () [] {
    goto other_state(count: 0);
}

state other_state (count: i15) [
    count: i15 = count;
] {
    $dsky::write_value(register: 0, value: count);
    yield $timer::wait(duration: 10);
    goto other_state(count: calculate_next(current: count));
}

sub calculate_next (current: i15) -> i15 {
    return current + 1;
}

```

## Getting Started

The compiler is written in rust. Build the `moonshot` compiler by running:

```bash
cargo build
```

The Moonshot compiler  will compile a program to AGC assembly code. The assembly can be assembled
into a virtual AGC program 'rope' by running the [yaYUL](https://www.ibiblio.org/apollo/yaYUL.html)
assembler to produce an exeuctable binary that can then be run with
[yaAGC](https://www.ibiblio.org/apollo/yaAGC.html).

A Dockerfile is provided that will bootstrap a container with the following tools installed:
 * The `moonshot` compiler
 * The `yaYUL` assembler
 * The `yaAGC` AGC emulator

With podman (or docker) installed, you can run a suite of integration tests that compile
moonshot programs, assemble them with `yaYUL`, and run them on `yaAGC`:

```bash
# Build the container and run the integration tests
podman build -t moonshot/test -f Dockerfile

# Drop into a shell in the container
podman run -ti moonshot/test /bin/sh
```

## Helpful Links
 * [The Virtual AGC Project](https://www.ibiblio.org/apollo/index.html) homepage
 * [Video log](https://youtu.be/VHDkWppysQI) of the development of another compiler targeting
   the AGC
