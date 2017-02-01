#!/bin/bash
export PATH=/home/rodrigoaf/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin.sh
Rscript --vanilla tuning.R $@ f1 svm true
