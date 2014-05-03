
Brief Overview
==============

Matkit is a tool (or more precisely a set of tools) to perform
analysis and arbitrary transformations on mathematical expressions. In
other words this is a symbolic computer algebra system, not a number
cruncher. Its main purpose is to help engineers in their every day
work. How this can be done? First of all, we'll define a terse and
expressive language for mathematical expressions. It is tedious to
write an expression like: $\mathbf{A}\mathbf{x}=\mathbf{b}$. 
Instead we should be able to write:

  Ax = b.

No more, no less. We will develop tools that will transform this
expression to other representations. Possible outputs are: latex,
mathml, matlab, png, fortran, c. Examples:

  echo "Ax=b." | matkit latex --use-package=amsmath

FINAL-NOTE: latex printing is not implemented.

will output a latex representation of the formula, using macros from
an amsmath package,

  echo "Ax=b." | matkit matlab --symbolic-mode

will output a formula in a matlab syntax, defining variables A,x and b
as symbols,

  echo "Ax=b." | matkit fortran --solve-for b

FINAL-NOTE: latex printing is not implemented.

will output a fortran program, containing a procedure that will solve
this equation for a b variable (i.e. perform a matrix multiplication).


Next, is analysis. Given the above expression, we can infer it's properties:

  echo "Ax=b." | matkit analyze

will output the following:
  Ax = b, 
  where A is in [n,m],
        x is in [m],
        b is in [n].

FINAL-NOTE: analysis is fully implemented, though the output slightly
differs, there should be capital letters.


That is a valid matlan expression, that can be put into
another matkit filter, something like this:

  echo "Ax=b." | matkit analyze | matkit latex

although a shortcut for the above can be provided:

  echo "Ax=b." | matkit latex --print-properties

(though it seems that shortcut is even longer...).

Since the declaration can be specified manually by a user, the analyzer
becomes a validator -- a tool to check the correctness of the
expression. Consider the following example:

  echo "Ax=b, 
        where A is in [n,m],
              b is in [m]". | matkit analyze 

will yield:

  Expression b is expected to have the following properties:
     b is in [n]
  but it is constrained with
     b is in [m]
  and
     m is not equal to n (syntactically).

FINAL-NOTE. Implemented as it is, module a message and type variables
case. Inded, we moved even futher - in a case of an error all
deriviation (that was made before the error occurres) is printed. 


Feature List
============


Matlan parser
-------------

Matlan is a terse and easy mathematical language, that should allow
one to specify complex mathematical expressions with as little code as
possible. It will consist of a lexer, able to process UTF lexemes
(we're planning to use ULex) and a parser that will transform a textual
representation of a mathematical object to an abstract syntax tree, so
that we can use our own language as glue between our components.

Typer
-----

This tool will infer properties of mathematical expressions with
respect to a set of constraints, provided by a user. It will use a
first-order E-unification algorithm, where E denotes a set of axioms
that defines an equivalence relation over expression types. By the
type of an expression we mean a set of properties that are true for a
given expression. For example, $A : {matrix in R[n,n]; symmetric}$ is
a most general type of the matrix A. Expressed in a matlan it will
look like:

  A is in R[n,n] and symmetric.

The hard problem here, is that usually unification algorithms are
expecting an expression to have a single (most general) type, but we
have a set of types. Whether we will infer them serially or all at
once is an open question.

Printer
-------

Printer will transform an AST to its textual representation
(i.e. antiparser). This approach has difficulties. For example the expression

          x'A'Ax

will be printed as

          (((x')*(A'))*A)*x.


This is not only ugly, but also incomprehensible. Obviously this is not
what we want.


Driver
------

Driver is not a tool, but a command line program that will unify
all the tools under one umbrella. Just like git or gcc.


Latex printer
-------------

This tool will extract a latex code from an abstract syntax tree. The
hard part about this tool is to make it flexible enough to satisfy
every user. 

FINAL-NOTE: Not implemented.


Simplifier/Optimizer
--------------------

This tool will apply a set of substitution rules (defined by algebra
axioms) on an expression in order to simplify it according to some
metrics. Given this definition we can write all our simplifiers and
optimizers as a functor parameterized by the cost function. Although,
it can be inefficient or even undecidable.

FINAL-NOTE: Not implemented.


BLAS/LAPACK compiler
--------------------

This tool will transform an AST to an intermediate language of BLAS
and LAPACK expressions, that can be considered as a mathematical
assembler language. A set of translators from assembler to a concrete
language implementation can be developed, like C translator, or
fortran translator. 

Techical Specification
======================

We will adopt a structure common to compilers. A front-end, that is
splitted in a parser and a typer. We will not do any work on a
middle-end (optimizer/simplifier) in the next two weeks. And we will
only produce some stub code for a back-end, e.g., a printer and latex
printer will state for a backend. An Ast will serve as a glue between
different parts of our project. 

Parser
------ 

We're using menhir as a parser generator since it accepts a broader
set of grammars, than a default one ocamlyacc. Moreover menhir
provides a human readable description of conflicts, - a feature we
find very helpful. 

Typer
----- 

We're going to implement type inference in a gradual manner. Starting
from a very simplistic and rather trivial we will move to a much more
sophisticated solution. We're going to split typer in a two separate
parts, the first part will generate a set of constraints, and the
second part will solve them using unification algorithm. This
separation will, hopefully, help us to deal with under-constrained
systems, in which case we can just enumerate all possible constraints
to generate a set of feasible solutions, that can be further analyzed
by a user. 

FINAL-NOTE: Yes, this works as described.



Detailed Design
===============

In this section we will introduce detailed designed of our
project. Most modules are well documented, so here we will just give a
general overview of a project structure. 

Ast
---

This is a central part of our project, where most types are specified. And
the central type is exp, that is used to represent an expression.

Exp
---

This module enhances exp type, implementing Hashable and Comparable
interfaces.  It also provides functions that build AST, such that we
can build examples of our expressions with a common OCaml syntax,
e.g. Exp.(tran x * a * x). This module also provides a folder that
abstracts a recursion over an AST tree. Although, We're going to
reimplement it as a class, that can be inherited.

Env
---

This module defines a mapping from expressions to their types. It is
responsible for creating fresh type variables for new expressions and
performing a lookup operations for the expressions that are already
registered in environment. All expressions are compared lexically. 

FINAL-NOTE: Renamed to Ctx, because of conflicts.

Type
----

This module is central to a typing infrastructure. It contains 
declarations for a type, constraints and substitution. 

Typing
------

This is an implementation of type inference algorithm. Only one
function is published. An infer function will infer type of an
expression with respect to constraints specified by a user. Indeed,
instead of single type a substitution is returned, that maps
expressions to their types. 

UnionFind
---------

This module implements a disjoint-set data structure, aka
UnionFind. It also renormalizes type variables. 

Lexer
-----

This module contains declarations of lexer.

Parser
------

This is a heart of our parser. It contains definitions, that are
processed by a parser generator. 



Progress Report
===============

We have a parser, that can cope with most of our grammar. Though we
have some issues with a juxtaposition. But in general it works fine. 
Some progress was achieved in a parsing branch.  We've finished
a constraint generator and unification algorithm. So, in general, type
inference works, but we still have open issues with overloading (and
we're not sure that they're solvable at all).    


Timeline
========

We have two weeks left, and here is our plan for them:

Week 1
------

- finish with parsing and cover parser with tests
- implement pretty-printer 
- cover typer with tests, fix bugs
- add support for overloading
- add support for unicode

Week 2
------

- implement driver program
- implement latex printer
- enhance usability (error messages and robustness)
- fixing bugs and polishing

Maybe, our plans aren't looking very ambitious, but we still have some open
issues to solve that can take a tremendous amount of time. This is
using a juxtaposition and overloading. 

FINAL-NOTE: Everything except latex printing is done. 


Further reading
===============

1. Project wiki [https://wiki.harvard.edu/confluence/display/fascs51matkit/]
2. Project page [https://code.seas.harvard.edu/matkit]
3. Team page    [https://code.seas.harvard.edu/+matkit]