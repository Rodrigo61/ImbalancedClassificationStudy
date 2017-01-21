##
# Script responsável por calcular a melhor performance a respeito
# das métricas de Acurácia, F1 e G-Means para os algoritmos SVM
# RF e XGboost. É realizado um CV k-fold e um randomSearch para
# hiperparametros. Alguns parametros sao setados ao inicio do 
# script
##


library(mlr)
library(stringr)
library(caret)
set.seed(3)

################################################################
####################  PARAMETROS DE CV  ########################
################################################################

#Quantas iteracoes serao feitas no random search
MAX_IT = 1L
#parametro K do K-folds
ITERS = 3L

DEBUG = F

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
table(dataset_path)
table(dataset[,'y_data'])

#Definindo Data frame com resultados para saida
out_df = NULL


################################################################
####################  FUNCOES     ##############################
################################################################
get_measures_from_tuneParams = function(search_space, dataset, learner_str, measure, weight_space=F){
  
  #Definindo variáveis de controle
  
  if(weight_space == T){
    MAJORITY_WEIGHT = length(which(dataset['y_data'] == 1))/length(which(dataset['y_data'] == 0))  
  }else{
    MAJORITY_WEIGHT = 1 #remove a influencia do cost learn
  }
  
  TRAIN_PERCENT = 0.8
  
  #Definindo configuracoes pro CV(k_fold  )
  ctrl = makeTuneControlRandom(maxit = MAX_IT)
  rdesc = makeResampleDesc("CV", iters = ITERS)
  
  #Definindo variavel de retorno da funcao
  result = NULL
  

  #Seperando treino e teste
  train_index = createDataPartition(1:length(dataset[,'y_data']), 
                                    times = 1,
                                    p = TRAIN_PERCENT,
                                    list = F)
  train = dataset[train_index,]
  test = dataset[-train_index,]

  #Realizando o tuning com a métrica escolhida
  res_tuneParams = tuneParams(learner_str, task = makeClassifTask(data=train, target='y_data'), resampling = rdesc,
                           par.set = num_ps, control = ctrl, measure=measure, show.info = DEBUG)
  result$performance_tuned = res_tuneParams$y
  
  #Treinando um modelo com o treino e os hiperparametros obtidos e armazenando a performance
  learner = setHyperPars(makeLearner(learner_str), par.vals = res_tuneParams$x)
  learner_res = mlr::train(learner, makeClassifTask(data=train, target='y_data'))
  p = predict(learner_res, task = makeClassifTask(data=test, target='y_data'))
  result$performance_trained = performance(p, measures = acc)

  return(result)
}


gen_all_measures_inline = function(search_space, dataset, learner_str, weight_space){
  
  measures_compilation = vector("list", ITERS)
  #Repetimos 3x a busca pelas performances
  for (i in 1:ITERS){
    #Realizando Tuning com métrica acurácia
    res_acc = get_measures_from_tuneParams(search_space = num_ps, dataset = dataset, learner_str = learner_str, measure = acc, weight_space = weight_space)
    #Realizando Tuning com métrica F1
    res_f1 = get_measures_from_tuneParams(search_space = num_ps, dataset = dataset, learner_str = learner_str, measure = f1, weight_space = weight_space)
    #Realizando Tuning com métrica gmean
    res_gmean = get_measures_from_tuneParams(search_space = num_ps, dataset = dataset, learner_str = learner_str, measure = gmean, weight_space = weight_space)
    #Realizando Tuning com métrica MCC
    res_gmean = get_measures_from_tuneParams(search_space = num_ps, dataset = dataset, learner_str = learner_str, measure = mcc, weight_space = weight_space)
    #sem weight space
    new_row = c(learner_str, weight_space, 
                res_acc$performance_tuned, 
                res_f1$performance_tuned, 
                res_gmean$performance_tuned,
                res_acc$performance_trained, 
                res_f1$performance_trained, 
                res_gmean$performance_trained, i)
    

    measures_compilation[[i]] = new_row
    
    if(DEBUG == T){
      print(learner_str)
      print("---")
      print("acc")
      print(res_acc$performance_tuned)
      print(res_acc$performance_trained)
      print("---")
      print("f1")
      print(res_f1$performance_tuned)
      print(res_f1$performance_trained)
      print("---")
      print("gmean")
      print(res_gmean$performance_tuned)
      print(res_gmean$performance_trained)
    }
  }
  return(measures_compilation)
}

################################################################
####################  TUNING SVM  ##############################
################################################################
#Definindo Search space
num_ps = makeParamSet(
  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10^x),
  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10^x)
)

#Gerando performance para o cenário normal e weight de treinamento
normal_learn = gen_all_measures_inline(search_space = num_ps, 
                                       dataset = dataset,
                                       learner_str = "classif.ksvm",
                                       weight_space = F)
weight_space_learn = gen_all_measures_inline(search_space = num_ps, 
                                             dataset = dataset,
                                             learner_str = "classif.ksvm",
                                             weight_space = T)

#Armazenando resultados das perfomances no dataframe final 
for(i in 1:ITERS){
  out_df = rbind(out_df, normal_learn[[i]])
  out_df = rbind(out_df, weight_space_learn[[i]])
}

################################################################
####################  TUNING RF  ###############################
################################################################

#Definindo Search space
num_ps = makeParamSet(
  makeDiscreteParam("mtry", c(1:(ncol(dataset)-1)))
)

#Gerando performance para o cenário normal e weight de treinamento
normal_learn = gen_all_measures_inline(search_space = num_ps, 
                                       dataset = dataset,
                                       learner_str = "classif.randomForest",
                                       weight_space = F)
weight_space_learn = gen_all_measures_inline(search_space = num_ps, 
                                             dataset = dataset,
                                             learner_str = "classif.randomForest",
                                             weight_space = T)

#Armazenando resultados das perfomances no dataframe final 
for(i in 1:ITERS){
  out_df = rbind(out_df, normal_learn[[i]])
  out_df = rbind(out_df, weight_space_learn[[i]])
}

################################################################
####################  GERANDO SAIDA  ###########################
################################################################

#Adicionando informacoes extras ao csv
colnames(out_df) = c("learner", "weight_space", 
                     "acc_tuned", "f1_tuned", 
                     "gmeans_tuned", "acc_trained", 
                     "f1_trained", "gmeans_trained", "time")
out_path = paste(str_sub(dataset_path, start = 1, end = -5), "_summary.csv", sep="")
write.table(out_df, out_path, col.names = T, row.names = F, sep=",")
