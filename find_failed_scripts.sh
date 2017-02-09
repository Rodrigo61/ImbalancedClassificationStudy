#!/bin/bash

PATH_2_ANALYZE=$1
PATTERN="halted"

grep -rwl $PATH_2_ANALYZE -e $PATTERN 
