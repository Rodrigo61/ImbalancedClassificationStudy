#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
Rscript --vanilla ../tuning.R --dataset_id=$@ --measure=mcc --model=rf --oversampling=smote
