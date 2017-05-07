#!/bin/bash

SUMMARY_LIST_FILENAME="summary_dir_list"

find $1 -type d -name "*summary*" > $SUMMARY_LIST_FILENAME

