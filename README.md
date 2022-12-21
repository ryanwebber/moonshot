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
module _ {
    import sys;

    proc main () -> () {
        let abc: i15 = 78;
        foo(bar: abc)
    }

    proc foo (bar: i15) -> () {
        // ...
    }
}
```

## Getting Started

The compiler is writted in rust. Build the `moonshot` compiler by running:

```bash
cargo install
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

## Useful Links
 * [The Virtual AGC Project](https://www.ibiblio.org/apollo/index.html) homepage
 * [Video log](https://youtu.be/VHDkWppysQI) of the development of another compiler targeting
   the AGC
