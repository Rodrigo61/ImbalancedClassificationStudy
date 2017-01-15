#!/bin/bash

for F in $(cat ./dataset_list) ; do
	echo "Executing "
	echo $F
	Rscript --vanilla tuningScript.R $F &
done;
