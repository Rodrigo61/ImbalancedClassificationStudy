#!/bin/bash

SUMMARY_LIST_FILENAME="summary_dir_list"

# Local
#UCI_PATH='/home/rodrigoaf/estudo_cost_learning/UCI/'

# RECOD
#UCI_PATH='/home/rodrigoaf/estudo_cost_learning/UCI/'

# JACQUES
UCI_PATH='/datasets/wainer/imbalance/UCI/'

find $UCI_PATH -type d -name "*summary*" > $SUMMARY_LIST_FILENAME

echo 'Arquivo '$SUMMARY_LIST_FILENAME' criado com sucesso'



