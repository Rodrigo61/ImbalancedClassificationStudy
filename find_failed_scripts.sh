#!/bin/bash

PATH_2_ANALYZE=$1

#grep -rwl $PATH_2_ANALYZE -e 'NaN' -e 'halted' -e 'error' -e 'warnings()'

#grep -rwl $PATH_2_ANALYZE -e 'NaN' -e 'halted' -e 'error'

#grep -rwl $PATH_2_ANALYZE -e 'halted' -e 'error'


#Erros conhecidos
# A ideia desse comando é detectar novos erros. A lista de erros conhecidossao:
# * Falta de memoria no SVM
# * Erro logico de IF no ADASYN
# * Erro Sigmoid
# * Erro de loop infinito (Muito raro)
grep -rL 'irrecoverable' $(grep -rL 'Sigmoid' $(grep -rL 'ADAS' $(grep -rL 'cannot allocate vector of size' $(grep -rwl $PATH_2_ANALYZE -e 'halted' -e 'error'))))







