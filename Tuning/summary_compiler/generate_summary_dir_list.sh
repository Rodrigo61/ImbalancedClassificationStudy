#!/bin/bash

SUMMARY_LIST_FILENAME="summary_dir_list"

# Local
#OUT_FILE_PATH='/home/rodrigoaf/estudo_cost_learning/UCI/'
# RECOD
OUT_FILE_PATH='/home/rodrigoaf/estudo_cost_learning/UCI/'

# Versao local
find $OUT_FILE_PATH -type d -name "*summary*" > $SUMMARY_LIST_FILENAME

echo 'Arquivo '$SUMMARY_LIST_FILENAME' criado com sucesso'



