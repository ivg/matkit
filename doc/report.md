

Introduction
============

We are proud to report that all core features of our project were
implemented. Although there're some features that were left
outside. The most crucial is error reporting. Parser and Lexer errors
aren't handled at all. Type error is handled, but can and should be
much more user friendly. 

Parsing
-------

We have implemented our every dare idea, including juxtaposition,
vocabulary definitions, property grouping, prefix and postfix
operators, subscripting, etc. Implementing juxtaposition was a real
challenge. We were forced to use right recursion in juxtaposition, and
this leads to some problems, such that expressions $A*B*C*D$ and
$ABCD$ produces different ast. The former is parsed as $((AB)C)D$,
and the later parses as $A(B(CD))$. Although they're mathematically
equal, this leads to some problems with pretty-printing.

Typing
------

The hardest part was to understand what is the type of matrix. Indeed,
in most papers unification algorithm is described in terms of computer
languages. And other papers are so abstract, that only authors can
understand their meaning. The good news is that at least we found a
proper definition for a type. The bad news, is that we started with a
bad representation... In matkit type algebra is defined in a following
"clever" way:

       dimension := variable
                  | constant
                  | number.

And matrix is a transformation from dimension to dimension. So, the type of
matrix is a pair of dimension. Since, all operations on matrices are
closed over a set of matrices, the type is defined simply as:
       
       type = dimension * dimension. 

Thus the constraint set can be described as set of pairs of type
dimension.

Another problem was that type variables was too volatile. And first
implementation didn't raise type errors, as any expression is a valid
if all terms are scalar. That's why we have added more rigid
variables, aka constants. Constants works just like numeric constants,
i.e. they unifies only with itself. Different variants of dimension
type are resolved on a lexical level, constants are uppercased,
variables lowercased, and numbers are represented by numerals.

Adding overloading in a presence of type-inference was a hard task,
and was solved partially. Although, we do not think that a full
solution exists. Overloading is only supported for expressions where
at least one term was explicitly annotated by a user as a scalar. This
means, that in expression like:

       x'AxB.

multiplication to the left of 'B' wouldn't be considered as a scalar
product. Indeed, this example is considered as a overuse of notation
even between mathematicians. 

Indeed, it is possible, to try to resolve overloading on later stages,
but in this case, the result of type inference will depend on a path
of unification. And we've decided that predictability is better, then
occasional «magical» overloading.


Pretty-printing
---------------

We decided to use treeprint library. The main issue with it, is that
the library weren't documented. So were forced to learn it from code
and unit tests. And our efforts were awarded. Pretty-printing with
combinators is really cool. Although, there was some problems with
printing our juxtapositioned multiplication (that was right
associative), but thanks to the power of combinators all problems were
solved. 

Driver
------

We used Core's Command library to drive our tookit. Only one command
is implemented at the moment of writing. But is easily extensible. 

Latex printer
-------------

Using treeprint library latex printer can be implemented with a few
keystrokes. Unfortunately, we haven't found a spare minute to
implement it. 

Simplifier/Optimzer and BLAS/Lapack compiler
--------------------------------------------

This features was always a far-future features, and was not intended
to implementation on first milestone. 


Teamworking
===========

Developing compiler is a teamwork, and it can be easily
paralleled. Most of the time each of us worked on its part of
a compiler. Thomas worked on parsing, Ivan on typing and support
modules. But last days, when we started integration process, were very harsh. 
An advice - use continuous integration, or at least integrate as soon
as possible. Do not postpone it to the last day. 

Conclusions
===========

When you develop parser, start with error handling. It is much harder
to add it later. And it will help you in debugging. 

Links
=====

1. A demo video [http://youtu.be/PZ087SIIPyE]
2. Original specification [doc/spec_final.md]