#!/bin/bash

for F in $(cat ./original_dataset_list) ; do
	echo "Executing "
	echo $F
	Rscript --vanilla transform.R $F &
done;
