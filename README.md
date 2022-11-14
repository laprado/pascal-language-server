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

## Extra configuration in LSP initialization options

Additional keys in LSP initialization options can be used to influence the LSP server behavior. See the docs of your LSP client (text editor) to know how to pass initialization options.

- `syntaxErrorCausesLspError` (boolean, behaves as `false` if not specified) : Report LSP error on syntax error when parsing Pascal file.

     By default, when this is `false`, the LSP server answers with a fake completion item with the error message. While it is a hack (we use completion item label to pass the error message), it works in VS Code and NeoVim.

     When this is `true`, the LSP server answers with LSP error. This is visible in Emacs.

- `syntaxErrorCausesShowMessage` (boolean, behaves as `true` if not specified) : Report a "show message" to LSP client on syntax error when parsing Pascal file.

     Note that this is independent from `syntaxErrorCausesLspError`. Regardless `syntaxErrorCausesLspError` (whether we respond with LSP error or fake item), we can also invoke a "show message" on LSP client.

     The effect of this depends on how the LSP client respects the `window/showMessage`.

     - VS Code shows it nicely.

     - Emacs shows it (but poorly, it will be quickly obscured by the message about lack of completions, and you will need to go to the `*Messages*` buffer to read it).

## Roadmap

### Wishlist

- Renaming of identifiers
- “Find all references”
- Signature help: Highlight active parameter
- Code formatting?

### Known bugs

- Does not work in include (`.inc`) files
- Signature help does not show all overloads
