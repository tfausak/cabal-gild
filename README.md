# Gild

[![CI](https://github.com/tfausak/cabal-gild/actions/workflows/ci.yml/badge.svg)](https://github.com/tfausak/cabal-gild/actions/workflows/ci.yml)
[![Hackage](https://badgen.net/hackage/v/cabal-gild)](https://hackage.haskell.org/package/cabal-gild)

Gild is an opinionated command line utility that formats Haskell package
descriptions, which are also known as `*.cabal` files. Gild can also be used to
automatically discover `exposed-modules`; see [the pragmas section][] for more
about that.

[the pragmas section]: #pragmas

Gild started as a fork of [phadej/cabal-fmt][], but is now totally separate.
For a brief summary of the differences between Gild and `cabal-fmt`, read [the
announcement post][].

[phadej/cabal-fmt]: https://github.com/phadej/cabal-fmt
[the announcement post]: https://taylor.fausak.me/2024/02/17/gild/

## Summary

Given a package description like this:

``` cabal
CABAL-VERSION : 3.0
name          : example
version       : 0.0.0.0

library
  build-depends: mtl>=2.3, base
  ghc-options:-Wall
  if impl(ghc>=9.8)
    ghc-options: -Wmissing-role-annotations
```

Gild will produce output like this:

``` cabal
cabal-version: 3.0
name: example
version: 0.0.0.0

library
  build-depends:
    base,
    mtl >=2.3,

  ghc-options: -Wall

  if impl(ghc >= 9.8)
    ghc-options: -Wmissing-role-annotations
```

See [the installation section][] for how to get Gild and [the usage section][]
for how to use it.

[the installation section]: #installation
[the usage section]: #usage

## Goals

- There should be no configuration options. As long as the output format is
  reasonable, the specifics should not matter too much. This means the amount
  of indentation cannot be changed, for example.

- The output should be diff friendly. This means things generally go on their
  own line, trailing commas are used when possible, and elements are sorted
  where it makes sense.

- The output should be semantically the same as the input. This means no
  normalization or canonicalization. For example, separate `build-depends`
  fields are not merged together.

- It should be possible to format other files that use the same format as
  package descriptions. This means `cabal.project` files can be formatted as
  well.

- The focus should be mostly on formatting. There should be no effort made to
  validate input or provide any checks or lints. However some quality of life
  features, like automatic module discovery, are desireable.

- Formatting should be as regular as possible. Special cases for particular
  fields or sections should be avoided unless it improves quality of life. For
  example, interpreting the `build-depends` field to pretty print it is okay.

- The command line utility should be fast enough to run on every save. It
  should not need network access.

## Installation

Go to [the latest release page][] and download the binary for your platform.

[the latest release page]: https://github.com/tfausak/cabal-gild/releases/latest

To run Gild in a GitHub Actions workflow, consider using [cabal-gild-setup-action][].

[cabal-gild-setup-action]: https://github.com/marketplace/actions/setup-gild

### From Source

In general you should prefer downloading the appropriate binary for you
platform. However it is possible to build Gild from source. It supports Linux,
macOS, and Windows along with the three most recent versions of GHC. Any other
configurations are unsupported.

With Cabal:

``` sh
$ cabal install cabal-gild
```

With Stack:

``` sh
$ stack install cabal-gild
```

## Usage

Gild is a command line utility named `cabal-gild`. By default it reads from
standard input (STDIN) and writes to standard output (STDOUT). Its behavior can
be modified with command line options, which are described below.

### Options

Run `cabal-gild --help` to see the options that Gild supports. They are:

- `--help`: Prints the help message to STDOUT then exits successfully.

- `--version`: Prints the version number to STDOUT then exits successfully.

- `--crlf=LENIENCY`: Sets the CRLF handling mode, which must be either
  `lenient` (the default) or `strict`. When checking if the input is formatted,
  setting this to `lenient` will treat CRLF line endings the same as LF.
  Setting this to `strict` will require the input to be byte-for-byte identical
  to the expected output. (Note that Gild will never produce CRLF line endings
  when formatting.)

- `--input=FILE`: Uses `FILE` as the input. If this is `-` (which is the
  default), then the input will be read from STDIN.

- `--io=FILE`: Shortcut for setting both `--input=FILE` and `--output=FILE` at
  the same time. This is useful for formatting a file in-place.

- `--mode=MODE`: Sets the mode to `MODE`, which must be either `format` (the
  default) or `check`. When the mode is `format`, Gild will output the
  formatted package description. When the mode is `check`, Gild will exit
  successfully if the input is already formatted, otherwise it will exit
  unsuccessfully.

- `--output=FILE`: Uses `FILE` as the output. If this is `-` (which is the
  default), then the output will be written to STDOUT. To modify a file in
  place, use the same file as both input and output. For example:

  ``` sh
  $ cabal-gild --input p.cabal --output p.cabal
  ```

  If the output is the same file as the input and the input is already
  formatted, then nothing will happen. The output will not be modified.

  It is an error to provide a value for this option when the mode is `check`.

- `--stdin=FILE`: When reading input from STDIN, use `FILE` as the effective
  input file. This is useful when a file's contents are already available, like
  in an editor. For example:

  ``` sh
  $ cabal-gild --stdin p.cabal < p.cabal
  ```

  It is an error to provide a value for this option unless the input is `-`.

### Pragmas

Gild supports special comments in package descriptions that act as pragmas.
Each pragma starts with `-- cabal-gild:`. Pragmas must be the last comment
before a field.

#### `discover`

```
-- cabal-gild: discover [DIRECTORY ...] [--include=PATTERN ...] [--exclude=PATTERN ...]
```

This pragma will discover files in any of the given directories. If no
directories are given, defaults to `.` (the directory of the package
description). For example, given this input:

``` cabal
library
  -- cabal-gild: discover
  exposed-modules: ...
```

Assuming there is a single Haskell file at `Example.hs`, Gild will produce this
output:

``` cabal
library
  -- cabal-gild: discover
  exposed-modules: Example
```

This pragma works with the following fields:

- `asm-sources`
- `c-sources`
- `cxx-sources`
- `data-files`
- `exposed-modules`
- `extra-doc-files`
- `extra-source-files`
- `includes`
- `install-includes`
- `js-sources`
- `license-files`
- `other-modules`
- `signatures`

It will be ignored on all other fields. For the `exposed-modules`,
`other-modules`, and `signatures` fields, only files with the following
extensions will be discovered:

- `*.chs`
- `*.cpphs`
- `*.gc`
- `*.hs`
- `*.hsc`
- `*.hsig`
- `*.lhs`
- `*.lhsig`
- `*.ly`
- `*.x`
- `*.y`

For all other fields, files with any extension will be discovered.

Any existing files, modules, or signatures in the field will be ignored. The
entire field will be replaced. This means adding, removing, and renaming files
should be handled automatically.

Directories can be quoted if they contain spaces. For example:

``` cabal
library
  -- cabal-gild: discover "my modules"
  exposed-modules: ...
```

By default, all files in any of the given directories are considered for
discovery. To explicitly include only certain files, use the
`--include=PATTERN` option. For example:

``` cabal
library
  -- cabal-gild: discover --include=**/*Spec.hs
  other-modules: ...
```

Files can be excluded from discovery by using the `--exclude=PATTERN` option.
For example:

``` cabal
library
  -- cabal-gild: discover --exclude=**/*Spec.hs
  exposed-modules: ...
```

If a file would match both the `--include` pattern and the `--exclude` pattern,
it will be excluded.
