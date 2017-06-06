#!/bin/bash

# Remove erros de alocao de vetor
find $1 -type f -exec grep -q 'cannot allocate vector' '{}' \; -exec rm '{}' \;

find $1 -type f -exec grep -q 'csv.save' '{}' \; -exec rm '{}' \;

find $1 -type f -exec grep -q 'ADAS' '{}' \; -exec rm '{}' \;

find $1 -type f -exec grep -q '.SigmoidPredict' '{}' \; -exec rm '{}' \;

