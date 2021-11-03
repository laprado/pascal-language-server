# Pascal Language Server

An [LSP](https://microsoft.github.io/language-server-protocol/) server
implementation for Pascal variants that are supported by [Free
Pascal](https://www.freepascal.org/), including Object Pascal. It uses
[CodeTools](https://wiki.lazarus.freepascal.org/Codetools) from
Lazarus as backend.

Forked from [the original
project](https://github.com/arjanadriaanse/pascal-language-server). This fork
adds many new features and fixes several bugs.


## Features

- Code completion
- Signature help
- Go to declaration
- Go to definition
- Automatic dependency resolution for `.lpk` and `.lpr` files

## Known bugs

- Does not work in include (`.inc`) files
- Signature help does not show all overloads

## Wishlist

- Renaming of identifiers
- “Find all references”
- Signature help: Highlight active parameter
- Code formatting?
- Incremental updates? (Not sure if there would be any benefit)

## Clients

### NeoVim ≥ 0.5.0

*todo*

### Emacs

To use the server from `lsp-mode` in Emacs, install the separate
[`lsp-pascal`](https://github.com/arjanadriaanse/lsp-pascal) module. (Disclaimer: I don't
maintain this and have not tested it as I don't use Emacs)

### Other
Any editor that allows you to add custom LSP configurations should in theory work.

## Configuration

In order for Pascal Language Server to find all the units, it needs to know the locations of:

- the source files of the FPC standard library
- the FPC compiler executable
- the Lazarus install directory

In addition, it needs to know

- the OS you are compiling for
- the architecture you are compiling for

There are three ways to tell Pascal Language Server this information:

0. Do nothing. 

   If you have Lazarus installed, Pascal Language Server will try to automatically detect your Lazarus settings. This should generally work, but it's possible that Pascal Language Server can't find your configuration or ends up using the wrong file, especially if your config is stored in an unorthodox location. It will look in these locations in the following order (from top to bottom):
   - \<User settings directory\>/lazarus (e.g. `/home/user/.config/lazarus`)
   - \<User home directory\>/.lazarus (e.g. `/home/user/.lazarus`)
   - \<System settings directory\>/lazarus (e.g. `/etc/lazarus`)
   
   The exact paths depend on your operating system.

1. Set the environment variables:

   - `PP` — Path to the FPC compiler executable
   - `FPCDIR` — Path of the source code of the FPC standard library
   - `LAZARUSDIR` — Path of your Lazarus installation
   - `FPCTARGET` — Target OS (e.g. Linux, Darwin, ...)
   - `FPCTARGETCPU` — Target architecture (e.g. x86_64, AARCH64, ...)

   This overrides auto-detected settings.

2. Specify the locations via LSP `initializationOptions`. How this is done will depend on your client. The format is the following:
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

## Building

Requires Free Pascal Compiler version 3.2.0 and Lazarus version 2.0.8,
open the project file in Lazarus or use the commandline:

```sh
lazbuild pasls.lpi
```
