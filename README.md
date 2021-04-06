# Racket Code Similarity
Code Similarity Program for determining likeness between Racket source code.

### [Software Design Document](https://github.com/defCoding/racket-code-similarity/blob/master/design_doc/design_doc.pdf)

## What does it do?
Provided a Racket source file, this program will check through its database of Racket source code and find which pre-existing code the source file is most similar to.

## Setup Instructions
Clone the repository to your local machine. You will need both [Python3](https://www.python.org/downloads/release/python-394/) and [Racket](https://docs.racket-lang.org/pollen/Installation.html) installed on your machine and added to your PATH.

## How to Run
Run the `compare_code.py` file and provide the file that you which to analyze:

```bash
$ python compare_code.py path/to/file.rkt
```

On the first run, the script will compile the Racket parser located in `utils/parser/`, but will not need to do so on further calls.

## Sample Files
Some sample archetype and input files have already been provided. Each file is named `r#.rkt`, where `#` is a number. When running the tests, you should run:

```bash
$ python compare_code.py sample_inputs/r#.rkt
```

You should notice that any given `r#.rkt` file from `sample_inputs/` should be most similar to its identically named counterpart from `archetypes/`.
