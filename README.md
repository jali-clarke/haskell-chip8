# haskell-chip8

an implementation of a [CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) emulator in haskell using the [opcodes described on wikipedia](https://en.wikipedia.org/wiki/CHIP-8#Opcode_table)

## how to use

build with either `cabal` (using `hpack`) or `nix`.

```sh
$ haskell-chip8 --help

Usage: haskell-chip8 ROM_FILE_PATH [--max-stack-size MAX_STACK_SIZE]
  emulate execution of a CHIP-8 rom

Available options:
  ROM_FILE_PATH            path to rom file
  --max-stack-size MAX_STACK_SIZE
                           max stack size / recursion depth (default: 50)
  -h,--help                Show this help text
```

this will create a window through which you can interact with the rom execution.
use the keyboard keys `0` to `9` and `a` to `f` as described on the wikipedia page in the header above.
