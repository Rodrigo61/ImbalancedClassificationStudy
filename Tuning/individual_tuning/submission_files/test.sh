#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
Rscript --vanilla ../tuning.R --dataset_id=0 --measure=f1 --model=xgboost --weight_space --oversampling=adasyn

