#!/bin/bash 
export PATH=/home/rodrigoaf/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin.sh
Rscript --vanilla tuning.R --dataset_id=$@ --measure=acc --model=svm --weight_space
