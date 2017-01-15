##
# Script responsável por calcular a melhor performance a respeito
# das métricas de Acurácia, F1 e G-Means para os algoritmos SVM
# RF e XGboost. É realizado um CV k-fold e um randomSearch para
# hiperparametros. Alguns parametros sao setados ao inicio do 
# script
##


library(mlr)
library(stringr)
set.seed(3)

################################################################
####################  PARAMETROS DE CV  ########################
################################################################

#Quantas iteracoes serao feitas no random search
MAX_IT = 10L
#parametro K do K-folds
ITERS = 3L

DEBUG = T

################################################################
####################  LENDO DATASET   ##########################
################################################################

#Lendo lista dos datasets
dataset_list = read.csv("dataset_list", header=F)

#Selecionando dataset pela posicao na lista
args = commandArgs(trailingOnly=TRUE)
dataset_id = as.numeric(args[1]) + 1
dataset_path = as.character(dataset_list[dataset_id,])
dataset_dir = dirname(dataset_path)

#Carregando dataset
dataset = read.csv(dataset_path, header = T)

################################################################
####################  CONF DO CV  ##############################
################################################################
#Definindo o controle do search
ctrl = makeTuneControlRandom(maxit = MAX_IT)

#Definindo método de CV
rdesc = makeResampleDesc("CV", iters = ITERS)

MAJORITY_WEIGHT = length(which(dataset['y_data'] == 1))/length(which(dataset['y_data'] == 0))
################################################################
####################  TUNING SVM  ##############################
################################################################

#Definindo Search space
num_ps = makeParamSet(
  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10^x),
  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10^x)
)

#Realizando Tuning com métrica acurácia
res_acc_svm = tuneParams("classif.ksvm", task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                     par.set = num_ps, control = ctrl, measure=acc, show.info = DEBUG)


#Realizando Tuning com métrica F1
res_f1_svm = tuneParams("classif.ksvm", task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                    par.set = num_ps, control = ctrl, measure=f1, show.info = DEBUG)

#Realizando Tuning com métrica gmean
res_gmean_svm = tuneParams("classif.ksvm", task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                       par.set = num_ps, control = ctrl, measure=gmean, show.info = DEBUG )

if(DEBUG == T){
  print("SVM")
  print("---")
  print("acc_svm")
  print(res_acc_svm$y)
  print("---")
  print("f1_svm")
  print(res_f1_svm$y)
  print("---")
  print("gmean_svm")
  print(res_gmean_svm$y)
}
################################################################
####################  TUNING SVM  ##############################
################################################################

#Definindo Search space
num_ps = makeParamSet(
  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10^x),
  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10^x)
)

#Realizando Tuning com métrica acurácia
res_acc_svm_wg = tuneParams(makeWeightedClassesWrapper(makeLearner("classif.ksvm"), wcw.weight = MAJORITY_WEIGHT), task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                         par.set = num_ps, control = ctrl, measure=acc, show.info = DEBUG)


#Realizando Tuning com métrica F1
res_f1_svm_wg = tuneParams(makeWeightedClassesWrapper(makeLearner("classif.ksvm"), wcw.weight = MAJORITY_WEIGHT), task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                        par.set = num_ps, control = ctrl, measure=f1, show.info = DEBUG)

#Realizando Tuning com métrica gmean
res_gmean_svm_wg = tuneParams(makeWeightedClassesWrapper(makeLearner("classif.ksvm"), wcw.weight = MAJORITY_WEIGHT), task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                           par.set = num_ps, control = ctrl, measure=gmean, show.info = DEBUG )

if(DEBUG == T){
  print("SVM WEIGHT")
  print("---")
  print("acc_svm")
  print(res_acc_svm_wg$y)
  print("---")
  print("f1_svm")
  print(res_f1_svm_wg$y)
  print("---")
  print("gmean_svm")
  print(res_gmean_svm_wg$y)
}
################################################################
####################  TUNING RF  ###############################
################################################################

#Definindo Search space
num_ps = makeParamSet(
  makeDiscreteParam("mtry", c(1:(ncol(dataset)-1)))
)

#Realizando Tuning com métrica acurácia
res_acc_rf = tuneParams("classif.randomForest", task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                     par.set = num_ps, control = ctrl, measure=acc, show.info = DEBUG)


#Realizando Tuning com métrica F1
res_f1_rf = tuneParams("classif.randomForest", task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                    par.set = num_ps, control = ctrl, measure=f1, show.info = DEBUG)

#Realizando Tuning com métrica gmean
res_gmean_rf = tuneParams("classif.randomForest", task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                       par.set = num_ps, control = ctrl, measure=gmean, show.info = DEBUG )

if(DEBUG == T){
  print("RF")
  print("---")
  print("acc_RF")
  print(res_acc_rf$y)
  print("---")
  print("f1_rf")
  print(res_f1_rf$y)
  print("---")
  print("gmean_rf")
  print(res_gmean_rf$y)
}


################################################################
####################  TUNING RF WEIGHT  ########################
################################################################

#Definindo Search space
num_ps = makeParamSet(
  makeDiscreteParam("mtry", c(1:(ncol(dataset)-1)))
)

#Realizando Tuning com métrica acurácia
res_acc_rf_wg = tuneParams(makeWeightedClassesWrapper(makeLearner("classif.randomForest"), wcw.weight = MAJORITY_WEIGHT), task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                        par.set = num_ps, control = ctrl, measure=acc, show.info = DEBUG)


#Realizando Tuning com métrica F1
res_f1_rf_wg = tuneParams(makeWeightedClassesWrapper(makeLearner("classif.randomForest"), wcw.weight = MAJORITY_WEIGHT), task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                       par.set = num_ps, control = ctrl, measure=f1, show.info = DEBUG)

#Realizando Tuning com métrica gmean
res_gmean_rf_wg = tuneParams(makeWeightedClassesWrapper(makeLearner("classif.randomForest"), wcw.weight = MAJORITY_WEIGHT), task = makeClassifTask(data=dataset, target='y_data', positive='1'), resampling = rdesc,
                          par.set = num_ps, control = ctrl, measure=gmean, show.info = DEBUG )

if(DEBUG == T){
  print("RF WEIGHT")
  print("---")
  print("acc_RF")
  print(res_acc_rf_wg$y)
  print("---")
  print("f1_rf")
  print(res_f1_rf_wg$y)
  print("---")
  print("gmean_rf")
  print(res_gmean_rf_wg$y)
}


################################################################
####################  GERANDO SAIDA  ###########################
################################################################

svm_row = c(res_acc_svm$y, res_f1_svm$y, res_gmean_svm$y)
svm_wg_row = c(res_acc_svm_wg$y, res_f1_svm_wg$y, res_gmean_svm_wg$y)
rf_row = c(res_acc_rf$y, res_f1_rf$y, res_gmean_rf$y)
rf_wg_row = c(res_acc_rf_wg$y, res_f1_rf_wg$y, res_gmean_rf_wg$y)

out_df = NULL
out_df = rbind(out_df, svm_row)
out_df = rbind(out_df, svm_wg_row)
out_df = rbind(out_df, rf_row)
out_df = rbind(out_df, rf_wg_row)
colnames(out_df) = c("accuracy", "f1", "gmeans")
rownames(out_df) = c("SVM", "SVM_WG", "RF", "RF_WG")

out_path = paste(str_sub(dataset_path, start = 1, end = -5), "_summary.csv", sep="")
write.table(out_df, out_path, col.names = T, row.names = T, sep=",")
