#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

Rscript --vanilla ../tuning.R --dataset_id=13 --measure=auc --model=rusboost
Rscript --vanilla ../tuning.R --dataset_id=17 --measure=acc --model=rusboost  
Rscript --vanilla ../tuning.R --dataset_id=27 --measure=acc --model=rusboost  
Rscript --vanilla ../tuning.R --dataset_id=47 --measure=acc --model=rusboost  
Rscript --vanilla ../tuning.R --dataset_id=77 --measure=acc --model=rusboost  
Rscript --vanilla ../tuning.R --dataset_id=107 --measure=acc --model=rusboost  
Rscript --vanilla ../tuning.R --dataset_id=137 --measure=acc --model=rusboost  
Rscript --vanilla ../tuning.R --dataset_id=167 --measure=acc --model=rusboost  
