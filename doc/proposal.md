Introduction
============

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


Project structure
=================


A core
------

First of all we need a robust parser that will transform a textual
representation of a mathematical object to an abstract syntax tree, so
that we can use our own language as glue between our components. Next,
we need a framework to perform a transformations on AST, something
like camlp4. This will be the core part of our project. All other
tools can be added independently. 

Driver
------

Driver is not a tool, but a command line program that will unify
all the tools under one umbrella. Just like git or gcc.


Printer
-------

Printer will transform an AST to its textual representation
(i.e. antiparser). This approach has difficulties. For example the expression

          x'A'Ax

will be printed as

          (((x')*(A'))*A)*x.


This is not only ugly, but also incomprehensible. Obviously this is not
what we want.

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

Simplifier/Optimizer
--------------------

This tool will apply a set of substitution rules (defined by algebra
axioms) on an expression in order to simplify it according to some
metrics. Given this definition we can write all our simplifiers and
optimizers as a functor parameterized by the cost function. Although,
it can be inefficient or even undecidable.


Latex printer
-------------

This tool will extract a latex code from an abstract syntax tree. The
hard part about this tool is to make it flexible enough to satisfy
every user. 

BLAS/LAPACK compiler
--------------------

This tool will transform an AST to an intermediate language of BLAS
and LAPACK expressions, that can be considered as a mathematical
assembler language. A set of translators from assembler to a concrete
language implementation can be developed, like C translator, or
fortran translator. 

Road-map
========

Week 1
------

Finish specification, establish environment and configuration tools.

Week 2
------

Freeze requirements. Finish AST, write a printer.

Week 3
------

Finish with basic functionality, including:
  * parser and lexer
  * printer
  * AST processor
  * driver

Week 4
------
  Finish typer and one other tool (preferably LaTeX printer).


Further reading
===============

1. Project wiki [https://wiki.harvard.edu/confluence/display/fascs51matkit/]
2. Project page [https://code.seas.harvard.edu/matkit]
3. Team page    [https://code.seas.harvard.edu/+matkit]