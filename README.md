Uniform methods to access data in files.

Goals:

- same functions with identical semantics independent of representation

- all functions are total (or become so using `Maybe` or `Either`)

- performance is NOT a goal - once the program logic is confirmed and tested, performance improvements can be achieved based on observations. The goal for "uniform" is reducing the complexity for the designer of a program - performance improvement come when the logic is correct. 

Specifically:

- a file path can be given as `Filepath` (which is a string) or as a `Path`, which different types for directories and files and for absolute and relative path, to reduce confusing.

- functions to work on file path independent of representation

- functions to access file with either type of path representation

- operations are total; failures of file operations are signaled by ErrorT or Either returns.

Experimentally:

- `TypedFile.hs` tries to extend a type concept from the representation in memory to a representation on file (indicated by the file extension). It proposes a set of functions to read structured files into structured data - selected by the extensions.