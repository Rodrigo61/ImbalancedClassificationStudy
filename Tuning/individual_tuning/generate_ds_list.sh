#!/bin/bash


# Local PATH
#PATH='/home/rodrigo/Desktop/datasets_UCI'

# RECOD PATH
UCI_PATH='/home/rodrigoaf/estudo_cost_learning/UCI'

find $UCI_PATH -name "*ds_0.*" ! -name "*FALHOU*" ! -name "*summary*"
