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

will output a latex representation of the formula, using macros from
an amsmath package,

  echo "Ax=b." | matkit matlab --symbolic-mode

will output a formula in a matlab syntax, defining variables A,x and b
as symbols,

  echo "Ax=b." | matkit fortran --solve-for b

will output a fortran program, containing a procedure that will solve
this equation for a b variable (i.e. perform a matrix multiplication).


Next, is analysis. Given the above expression, we can infer it's properties:

  echo "Ax=b." | matkit analyze

will output the following:
  Ax = b, 
  where A is in [n,m],
        x is in [m],
        b is in [n].

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


Simplifier/Optimizer
--------------------

This tool will apply a set of substitution rules (defined by algebra
axioms) on an expression in order to simplify it according to some
metrics. Given this definition we can write all our simplifiers and
optimizers as a functor parameterized by the cost function. Although,
it can be inefficient or even undecidable.



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


Next steps
==========

We're already started coding. Our environment is fully set up.
Matlan grammar is specified. Ast is mostly ready with some supporting
code. We're actively working on typing, though we're still in an
active search for good representation for an expression
type. Especially for matrix kinds.   

Until the end of this week we're hoping to implement rather large
subset of our grammar, and a skeleton of a typer. Typer should at
least be able to infer (and check) dimensions of matrices.

Further reading
===============

1. Project wiki [https://wiki.harvard.edu/confluence/display/fascs51matkit/]
2. Project page [https://code.seas.harvard.edu/matkit]
3. Team page    [https://code.seas.harvard.edu/+matkit]