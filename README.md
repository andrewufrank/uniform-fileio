Uniform methods to access data on file

Uniform goals:
- same functions with identical semantics independent of representation
- all functions are total (or become so using Maybe or Either)

Specifically:
- a file path can be given as `Filepath` (which is a string) or as a `Path`, which different types for directories and files and for absolute and relative path, to reduce confusiong.
- functions to work on file path independent of representation
- functions to access file with either type of path representation

Experimentally:
- TypedFile.hs tries to extend a type concept from the represenation in memory to a representation on file (indicated by the file extension). It proposes a set of functions to read structured files into structured data - selected by the extensions.