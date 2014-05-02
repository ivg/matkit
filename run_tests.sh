#!/bin/sh

./test_parsing.native &&
./test_typing.native  &&
./test_decl.native    &&
./test_exp.native     &&
(cd src/examples; sh selftest.sh) && echo "All tests passed" || echo "Error"
