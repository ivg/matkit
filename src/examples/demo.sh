#!/bin/sh

for slide in `ls slide*.mtk`; do
    clear
    cat $slide;
    read
    echo "============================"
    ../../matkit.native infer -i $slide
    read 
done 
