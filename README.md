
# Secret Santa Assigner

Reads a list of friends from the standard input and renders to a specified
output file (see --help) all assignments of gift givers and their corresponding
receivers encoded in a QR-Code.

## Requisites

 * [Haskell](http://haskell.org)
 * [Diagrams](http://projects.haskell.org/diagrams/)

## Example usage:

    $ runhaskell secret.hs -o output.pdf << EOF
    Charlotte
    Grayson
    Elliot
    Benjamin
    Willian
    EOF

### Output

![Image](example.png)

