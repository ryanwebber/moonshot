# Moonshot Language + Toolchain

A compiler toolchain targeting the Apollo Guidance Computer. This is a hobby
project started to learn more about the AGC architecture and software.

Eventually, it would be fantastic if a guidance program written and compiled by moonshot
could be loaded into an AGC simulation and successfully perform an automated moon landing.

### Progress

The compiler has enough features to compile a program with a single main function. Only
the 15-bit integer type is supported, and addition is the only arithmetic operation
supported. While this set of features isn't enough to do anything useful, a decent amount
of compiler architecture is implemented in order to begin implementing more language features
and compiler checks.

## Example

```
// main.mns
inc "progs/write_mem.mns" as write_mem
inc "progs/test_multistate.mns" as test_multistate

verb 99 "debug" {
    noun 01 "write" {
        .entry = write_mem::main;
    }
}

verb 98 "test" {
    noun 00 "multistate" {
        .entry = test_multistate::main;
    }
}

// progs/write_mem.mns
state main () [
    addr: i15 = 0;
    value: i15 = 0;
] {
    $dsky::read_value (&addr);
    $dsky::read_value (&value);
    $mem::write (addr, value);
}

// progs/test_multistate.mns
state main () [] {
    .goto ::other_state (0)
}

state other_state (count: i15) [
    count: i15 = count;
] {
    .goto ::other_state(::calculate_next(count));
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

With docker installed, you can enter this environment with the following sequence of commands:

```bash
# Create the docker container
docker build -t moonshot-agc .

# Drop a shell into the container
docker run --rm -it --entrypoint sh moonshot-agc

# Compile, assemble, and execute a program
moonshot > main.agc
yaYUL main.agc
yaAGC main.agc.bin
```

## Helpful Links
 * [The Virtual AGC Project](https://www.ibiblio.org/apollo/index.html) homepage
 * [Video log](https://youtu.be/VHDkWppysQI) of the development of another compiler targeting
   the AGC
