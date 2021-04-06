from abc import ABC, abstractmethod

#################################
#        Abstract Classes       #
#################################

class AST(ABC):
    def __init__(self):
        self.components = []
        self.similarity_cap = -1

    @abstractmethod
    def compare(self, other):
        """
        Returns the similarity score between this AST node and another.

        Params:
        - other (AST) : the other ast to compare to

        Returns:
        - (number) : the similarity score
        """
        pass

    def similarity_cap(self):
        """
        Returns the similarity cap of this node.

        Returns:
        - (number) : the similarity cap
        """
        if similarity_cap == -1:
            similarity_cap = 1 + sum([c.similarity_cap() for c in self.components])
        
        return similarity_cap

    def relative_similarity(self, other):
        """
        Calculates the relative similarity of two ASTs.

        Params:
        - other (AST) : the other ast to compare to

        Returns:
        - (number) : the relative similarity
        """
        return self.compare(other) / min(self.similarity_cap(), other.similarity_cap())

class Atom(AST):
    @abstractmethod
    def compare(self, other):
        pass

    def similarity_cap(self):
        return 2

class Exp(AST):
    @abstractmethod
    def compare(self, other):
        pass


#################################
#         Atom Classes          #
#################################

class Int(Atom):
    def __init__(self, n):
        self.n = n

    def compare(self, other):
        if type(other) is Int:
            return 1 + (1 if self.n == other.n else 0)
        
        return 0

class Bool(Atom):
    def __init__(self, b):
        self.b = b

    def compare(self, other):
        if type(other) is Bool:
            return 1 + (1 if self.b == other.b else 0)

        return 0

class Str(Atom):
    def __init__(self, s):
        self.s = s

    def compare(self, other):
        if type(other) is Str:
            return 1 + (1 if self.s == other.s else 0)

        return 0

class Op(Atom):
    def __init__(self, o):
        self.o = o

    def compare(self, other):
        if type(other) is Op:
            return 1 + (1 if self.o == other.o else 0)

        return 0

class Var(Atom):
    def __init__(self, v):
        self.v = v

    def compare(self, other):
        if type(other) is Var:
            return 1 + (1 if self.v == other.v else 0)

        return 0

class Empty(Atom):
    def compare(self, other):
        return 2 if type(other) is Empty else 0


#################################
#          Exp Classes          #
#################################

class If(Exp):
    def __init__(self, con, consq, alter):
        self.con = con
        self.consq = consq
        self.alter = alter
        self.components = [con, consq, alter]

    def compare(self, other):
        if type(other) is If:
            # Compare condition
            con_score = self.con.compare(other.con)

            # Do two sets of comparisons for consequent and alternative, and pick the larger.
            mirror_score = self.consq.compare(other.consq) + self.alter.compare(other.alter)
            alt_score = self.consq.compare(other.alter) + self.alter.compare(other.consq)

            return con_score + max((mirror_score, alt_score))

        return 0

class Let(Exp):
    def __init__(self, v, val, body):
        self.v = v
        self.val = val
        self.body = body
        self.components = [val, body]

    def compare(self, other):
        if type(other) is Let:
            # Compare value expressions
            val_score = self.val.compare(other.val)

            # Compare bodies
            body_score = self.body.compare(other.body)

            return val_score + body_score

        return 0

class Apply(Exp):
    def __init__(self, fn, args):
        self.fn = fn
        self.args = args
        self.components = [fn, args]

    def compare(self, other):
        if type(other) is Apply:
            func_similarity = self.fn.compare(other.fn)
            pairings, excess_args = pair_up_similar_asts(self.args, other.args)

            return max(func_similarity + sum(pairings.values()) - sum(map(lambda a : a.similarity_cap(), excess_args)), 0)

        return 0

class Lambda(Exp):
    def __init__(self, p_ls, body):
        self.p_ls = p_ls
        self.body = body
        self.components = [body]

    def compare(self, other):
        if type(other) is Lambda:
            return self.body.compare(other.body) - abs(len(self.p_ls) - len(other.p_ls))

        return 0

class Def(AST):
    def __init__(self, fn_name, p_ls, body):
        self.fn_name = fn_name
        self.p_ls = p_ls
        self.body = body
        self.components = [body]

    def compare(self, other):
        if type(other) is Def:
            return self.body.compare(other.body) - abs(len(self.p_ls) - len(other.p_ls))

        return 0

class Program(AST):
    def __init__(self, def_ls, exp_ls):
        self.def_ls = def_ls
        self.exp_ls = exp_ls
        self.components = def_ls + exp_ls

    def compare(self, other):
        if type(other) is Program:
            def_pairings, excess_def = pair_up_similar_asts(self.def_ls, other.def_ls)
            exp_pairings, excess_exp = pair_up_similar_asts(self.exp_ls, other.exp_ls)

            return sum(def_pairings.values()) + sum(exp_pairings.values())

def pair_up_similar_asts(ls1, ls2):
    """
    Given two lists of AST, pair up AST.

    Params:
    - ls1 (List of AST) : List of ASTs.
    - ls2 (List of AST) : The other list of ASTs.

    Returns:
    - (Tuple of ((Dict of (Tuple of (AST, AST)) : (number)), (List of AST))) : 
        Returns a tuple with two items, a dictionary and a list.
        The dictionary maps tuple pairings of ASTs to their similarity score.
        The list contains all extra ASTs that were not able to be matched.
    """
    # Determine which list is the smaller list and which is the larger.
    smaller, larger = (ls1, ls2) if len(ls1) <= len(ls2) else (ls1, ls2)

    results = {}
    for ast1 in smaller:
        # Pairings contain the data for each pairing between ast1 and each ast in larger.
        pairings = [[index, ast2, ast1.compare(ast2)] for index, ast2 in enumerate(larger)]

        # Pick the best pairing by picking the largest similarity score.
        best_pair = max(pairings, key=lambda p : p[2])

        # Remove the paired ast from the list.
        larger.pop(best_pair[0])

        results[(ast1, best_pair[1])] = best_pair[2]

    # Return results and remaining items in the larger list.
    return (results, larger)
