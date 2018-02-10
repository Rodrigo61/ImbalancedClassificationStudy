#!/bin/bash

# Local PATH
#UCI_PATH='/home/rodrigo/Desktop/datasets_UCI'

# RECOD PATH
#UCI_PATH='/home/rodrigoaf/estudo_cost_learning/UCI'

# JACQUES
UCI_PATH='/datasets/wainer/imbalance/UCI/'

find $UCI_PATH -name "*ds_0.*" ! -name "*FALHOU*" ! -name "*summary*"
