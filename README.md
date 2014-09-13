
# Secret Santa Assigner

Reads a list of friends from the standard input (or a file) and renders to a
specified output file (or "output.pdf" if not specified) all assignments of
gift givers and their corresponding receivers encoded in a QR-Code (see --help
for options).

## Installation

To build and install you will need Haskell's
[Cabal](http://www.haskell.org/cabal/), then just do:

```console
$ cabal install
```

It will automatically download the dependencies, compile, and install the
executable `secret-santa` in the Cabal bin path (most likely `~/.cabal/bin`).

## Example usage

```console
$ secret-santa -h
Usage: secret-santa [OPTION...]
  -o FILE  --output=FILE    output FILE
  -i FILE  --input=FILE     input FILE
  -h       --help, --usage  This help message
$ secret-santa -o output.pdf << EOF
Charlotte
Grayson
Elliot
Benjamin
Willian
EOF
```

### Example output

![Image](example.png)

