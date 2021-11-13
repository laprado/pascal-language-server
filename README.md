# Pascal Language Server

An [LSP](https://microsoft.github.io/language-server-protocol/) server
implementation for Pascal variants that are supported by [Free
Pascal](https://www.freepascal.org/). It uses
[CodeTools](https://wiki.lazarus.freepascal.org/Codetools) from Lazarus as
backend.

Forked from [the original
project](https://github.com/arjanadriaanse/pascal-language-server), but has
since been mostly rewritten. This fork adds many new features and fixes several
bugs.


## Features

- Code completion
- Signature help
- Go to declaration
- Go to definition
- Automatic dependency resolution for `.lpk` and `.lpr` files

## Building

First, make sure, submodules are loaded:
```
git submodule update --init --recursive
```

To compile, open the project file in Lazarus or use the command line:

```sh
cd server
lazbuild pasls.lpi
```

It is recommended to use Free Pascal Compiler version 3.2.0 and Lazarus version
2.0.8 or later, older versions are not officially supported.

## Clients

### Neovim ≥ 0.5.0

For information on how to use the server from Neovim, see [client/nvim](client/nvim).

### Emacs

To use the server from `lsp-mode` in Emacs, install the separate
[`lsp-pascal`](https://github.com/arjanadriaanse/lsp-pascal) module.
(Disclaimer: I don't maintain this and have not tested it as I don't use Emacs)

### Other
Any editor that allows you to add custom LSP configurations should work.

## Configuration

In order for the language server to find all the units, it needs to know the
following parameters:

- location of the FPC standard library source files
- location of the FPC compiler executable
- location of the Lazarus install directory
- the OS you are compiling for
- the architecture you are compiling for

By default, the server will try to auto-detect these parameters from your
Lazarus config. It will search for config files in the following locations (the
exact paths will depend on your operating system):

- `<User settings directory>/lazarus` (e.g. `/home/user/.config/lazarus`)
- `<User home directory>/.lazarus` (e.g. `/home/user/.lazarus`)
- `<System settings directory>/lazarus` (e.g. `/etc/lazarus`)

In addition, you can also specify these parameters manually in one of the
following ways:

1. Set the environment variables:

   - `PP` — Path to the FPC compiler executable
   - `FPCDIR` — Path of the source code of the FPC standard library
   - `LAZARUSDIR` — Path of your Lazarus installation
   - `FPCTARGET` — Target OS (e.g. Linux, Darwin, ...)
   - `FPCTARGETCPU` — Target architecture (e.g. x86_64, AARCH64, ...)

   This overrides auto-detected settings.

2. Or specify the locations via LSP `initializationOptions`. How this is done
   will depend on your client. The format is the following:
   ```json
   {
     "PP": "",
     "FPCDIR": "",
     "LAZARUSDIR": "",
     "FPCTARGET": "",
     "FPCTARGETCPU": ""
   }
   ```

   This overrides environment variables.

## Roadmap

### Wishlist

- Renaming of identifiers
- “Find all references”
- Signature help: Highlight active parameter
- Code formatting?

### Known bugs

- Does not work in include (`.inc`) files
- Signature help does not show all overloads
