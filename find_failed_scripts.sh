#!/bin/bash

PATH_2_ANALYZE=$1

#grep -rwl $PATH_2_ANALYZE -e 'NaN' -e 'halted' -e 'error' -e 'warnings()'

#grep -rwl $PATH_2_ANALYZE -e 'NaN' -e 'halted' -e 'error'

grep -rwl $PATH_2_ANALYZE -e 'halted' -e 'error'

#grep -rwl $PATH_2_ANALYZE  -e 'mcc'




