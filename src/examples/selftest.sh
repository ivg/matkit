#!/bin/sh

# this test checks that matkit can eat its own dogfood,
# i.e. can parse its own output.


for f in `ls ex*.mtk`; do
    echo "checking $f"
    ../../matkit.native infer -i $f | ../../matkit.native infer && echo "ok" || echo "failed"
done


