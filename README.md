# Usage

To cite using `cite-apa`, you first need to create a reference library.

A reference library is a directory containing `.ref` files.

For example, my current library:

```
❯ ls .local/share/cite-apa/
cs-1101.ref  example.ref
```

Each one of those `.ref` files can contain multiple entries of resources, for example:

```
❯ cat .local/share/cite-apa/cs-1101.ref 
book Think Python: How to think like a computer scientist
    author Allen Downey
    edition 2
    publisher Green Tea Press
    location Needham, MA
    year 2015

book The C Programming Language
    author Brian Kernighan
    author Dennis Ritchie
    publisher Prentice Hall
    location Englewood Cliffs, NJ
    year 1978

...
```

When you find an work that you may later want to cite, you need to write a new entry in
your reference library, then it will be available from inside EMACS for easy citation.
