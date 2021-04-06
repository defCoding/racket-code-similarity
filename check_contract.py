from lib.ast import *
from subprocess import Popen, PIPE
from os import path, listdir
import sys

if __name__ == '__main__':
    # Check for correct number of command line arguments.
    if len(sys.argv) < 2:
        raise TypeError('Please provide the file you would like to check.')
    elif len(sys.argv) > 2:
        print('Only one file can be checked at a time. Excess file arguments have been discarded.')

    # Check if file exists.
    if not path.exists(sys.argv[1]):
        raise FileNotFoundError(f'No such file: {sys.argv[1]}')


    # Collect all archetypal source code (e.g. pre-existing validated and mature smart contracts). Map filename to source code.
    archetypes = {}
    for filename in listdir('archetypes/'):
        filename = f'archetypes/{filename}'
        if path.isfile(filename):
            # Create process for converting Racket source code into Python AST object.
            constructor_process = Popen(['./utils/parser/parser', filename], stdout=PIPE)
            constructor_str = constructor_process.communicate()[0].decode()
            archetypes[filename] = eval(constructor_str)

    # Construct AST for file to be checked.
    constructor_process = Popen(['./utils/parser/parser', sys.argv[1]], stdout=PIPE)
    constructor_str = constructor_process.communicate(input=sys.argv[1])[0].decode()
    ast_to_check = eval(constructor_str)

    # Find the most similar archetype file.
    highest_relative_similarity = -1
    most_similar_similarity_score = -1
    most_similar_filename = ""
    for filename, archetype_ast in archetypes.items():
        similarity_score, relative_similarity = ast_to_check.code_similarity(archetype_ast)

        if relative_similarity > highest_relative_similarity:
            highest_relative_similarity = relative_similarity
            most_similar_similarity_score = similarity_score
            most_similar_filename = filename

    # Output results.
    print(f"""
File {sys.argv[1]} was most similar to the archetype file {most_similar_filename}.

Relative Similarity: {highest_relative_similarity}
Similarity Score: {most_similar_similarity_score}
""")
