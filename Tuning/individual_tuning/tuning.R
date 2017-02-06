##
# Script responsável por calcular a melhor performance a respeito
# das métricas para cada algoritmo desejado. É realizado um CV k-fold e um randomSearch para
# hiperparametros. Alguns parametros sao setados ao inicio do 
# script. O script deve ser extensivel e totalmente independente, de modo que o tuning
# eh feito apenas para uma das possiveis combinacoes de metrica X algoritmo X cenario por vez.
##


library(mlr)
library(stringr)
library(caret)
library("optparse")
set.seed(3)

#**************************************************************#
#*******************  CONSTANTES   ****************************#
#**************************************************************#

#Quantas iteracoes serao feitas no random search
MAX_IT = 15L
#parametro K do K-folds
ITERS = 3L
DEBUG = T
SVM_STR = "classif.ksvm"
RF_STR = "classif.randomForest"
XGBOOST_STR = "classif.xgboost" 
SUMMARY_FOLDER_NAME = "summary_files"
DATASET_LIST_PATH = "../dataset_list"
COLUMNS_NAMES = c("learner", "weight_space", 
                  "tuning_measure", "holdout_measure",
                  "iteration_count")

#**************************************************************#
#*******************  FUNCOES     ******************************
#**************************************************************#

get_args = function(){
  description = " Script responsável por calcular a melhor performance a respeito
     das métricas para cada algoritmo desejado. É realizado um CV k-fold e um randomSearch para
     hiperparametros. Alguns parametros sao setados ao inicio do 
     script. O script deve ser extensivel e totalmente independente, de modo que o tuning
     eh feito apenas para uma das possiveis combinacoes de metrica X algoritmo X cenario por vez"
  
  option_list = list(
    make_option(c("--dataset_id"), type="integer", default=NULL, 
                help="métrica utilizada para otimizacao"),
    make_option(c("--measure"), type="character", default=NULL, 
                help="métrica utilizada para otimizacao"),
    make_option(c("--model"), type="character", default=NULL, 
                help="algoritmo que será realizado o tuning"),
    make_option(c("--weight_space"), action= "store_true", default=NULL, 
                help="se presente a flag o treinamento será feito com weight_space")
  )
  
  opt_parser = OptionParser(option_list=option_list, description = description)
  return(parse_args(opt_parser))
}

#----------------------#
get_measures_from_tuneParams = function(search_space, dataset, learner_str, measure, weight_space=F){
  
  #Definindo variáveis de controle
  
  if(weight_space == T){
    MAJORITY_weight = length(which(dataset['y_data'] == 1))/length(which(dataset['y_data'] == 0))  
  }else{
    MAJORITY_weight = 1 #remove a influencia do cost learn
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
                           par.set = search_space, control = ctrl, measure=measure, show.info = DEBUG)
  result$performance_tuned = res_tuneParams$y
  
  #Treinando um modelo com o treino e os hiperparametros obtidos e armazenando a performance
  learner = setHyperPars(makeLearner(learner_str), par.vals = res_tuneParams$x)
  learner_res = mlr::train(learner, makeClassifTask(data=train, target='y_data'))
  p = predict(learner_res, task = makeClassifTask(data=test, target='y_data'))
  result$performance_holdout = performance(p, measures = acc)

  return(result)
}

#----------------------#
print_debug = function(str){
  if(!is.character(str)){
    str = paste(str, collapse = "")
  }
  if(DEBUG){
    print(paste("[DEBUG]", str, sep=""))
  }
}

#----------------------#
gen_all_measures_inline = function(search_space, dataset, learner_str, weight_space, measure){
  
  measures_compilation = vector("list", ITERS)
  #Repetimos 3x a busca pelas performances
  for (i in 1:ITERS){
    
    #Realizando Tuning com métrica acurácia
    measures = get_measures_from_tuneParams(search_space = search_space, 
                                           dataset = dataset, 
                                           learner_str = learner_str, 
                                           measure = measure, 
                                           weight_space = weight_space)
  
    new_row = c(learner_str, weight_space, measures$performance_tuned, 
                measures$performance_holdout, i)
    
    measures_compilation[[i]] = new_row
    
  }
  return(measures_compilation)
}

#----------------------#
select_measure = function(arg){
  if(is.na(arg) | is.null(arg)){
    warning("Nao foi informado a metrica desejada")
    stop()
  }
  
  if(arg == "acc"){
    return(acc)
  }else if(arg == "f1"){
    return(f1)
  }else if(arg == "gmeans"){
    return(gmeans)
  }else if(arg == "mcc"){
    return(mcc)
  }else{
    warning("Selecione uma das seguintes metricas: acc, f1, gmeans, mcc")
    stop()
  }
}

#----------------------#
select_learner = function(arg){
  if(is.na(arg) | is.null(arg)){
    warning("Nao foi informado o algoritmo desejado")
    stop()
  }
  
  if(arg == "svm"){
    return(SVM_STR)
  }else if(arg == "rf"){
    return(RF_STR)
  }else if(arg == "xgboost"){
    return(XGBOOST_STR)
  }else{
    warning("Selecione um dos seguintes algoritmos: svm, rf")
    stop()
  }
}

#----------------------#
select_weight_space = function(arg){
  if(is.null(arg)){
   return(F)
  }else{
   return(T)
  }
}

#----------------------#
select_search_space = function(learner_str){
  if(learner_str == SVM_STR){
    return(
      makeParamSet(
        makeNumericParam("C", lower = 2**(-5), upper = 2**15),
        makeNumericParam("sigma", lower = 2**(-15), upper = 2**3)
      )
    )
  }else if(learner_str == RF_STR){
    return(
      makeParamSet(
        makeDiscreteParam("mtry", c(1:(ncol(dataset)-1))),
        makeDiscreteParam("ntree", c((2**4):(2**12)))
      )
    )
  }else if(learner_str == XGBOOST_STR){
    return(
      makeParamSet(
        makeDiscreteParam("max_depth", c(1:6)),
        makeNumericParam("eta", lower=0.005, upper = 0.5),
        makeDiscreteParam("nrounds", c(20:150))
      )
    )
  }else{
    warning(paste("Nao existe um search_space definido para o algoritmo ", learner_str, sep=""))
    stop()
  }
}

#----------------------#
exec_tuning = function(dataset, learner_str, measure, weight_space){
  
  search_space = select_search_space(learner_str)
  
  tuning_and_holdout = gen_all_measures_inline(search_space = search_space, 
                                               dataset = dataset,
                                               learner_str = learner_str,
                                               weight_space = weight_space,
                                               measure = measure)
  
  print_debug("Resultados do tuning:")
  print_debug(paste(COLUMNS_NAMES, collapse=" | "))
  print(tuning_and_holdout)
}

#----------------------#
save_tuning = function(measure_list, dataset_path, dataset_imba_rate, learner_str, measure, weight_space){
  
  #compilando a lista de metricas em um unico dataframe
  out_df = NULL
  for(i in 1:ITERS){
    out_df = rbind(out_df, measure_list[[i]])
  }
  
  colnames(out_df) = COLUMNS_NAMES
  
  #Criando caso nao exista a pasta para salvar os arquivos com os resultados
  dirname = paste(SUMMARY_FOLDER_NAME, as.character(dataset_imba_rate), sep="")
  dir.create(file.path(dirname(dataset_path), dirname), showWarnings = DEBUG)
  
  #Salvando dados
  out_filename = paste(learner_str, measure$name, as.character(weight_space), sep ="_")
  out_path = str_replace_all(paste(dirname(dataset_path), paste(dirname, out_filename, sep="/"), sep="/"), " ", "_")
  write.table(out_df, out_path, col.names = T, row.names = F, sep=",")
  print_debug(paste("Tuning salvo em: ", out_path, sep=""))
}

#**************************************************************#
#*******************  MAIN   **********************************#
#**************************************************************#

print_debug(getwd())

#Lendo os parametros o script
opt = get_args()

#Lendo lista dos datasets
dataset_list = read.csv(DATASET_LIST_PATH, header=F)

#Selecionando dataset pela posicao na lista
dataset_id = as.numeric(opt$dataset_id) + 1
dataset_path = as.character(dataset_list[dataset_id,])
dataset_dir = dirname(dataset_path)
dataset_imba_rate = str_extract(dataset_path, "0.[0-9]{2,3}")

#Selecionando os parametros para o tuning
measure = select_measure(opt$measure)
learner_str = select_learner(opt$model)
weight_space = select_weight_space(opt$weight_space)

#Carregando dataset
dataset = read.csv(dataset_path, header = T)
table(dataset_path)
table(dataset[,'y_data'])

#Executando e armazenando os valores obtidos com o tuning
print_debug("Executando o tuning com os seguintes parametros:")
print_debug(paste("Dataset: ", dataset_path))
print_debug(paste("Algoritmo: ", learner_str))
print_debug(paste("Metrica: ", measure$name))
print_debug(paste("Weitgh space: ", weight_space))

measure_list = exec_tuning(dataset = dataset, 
                           learner_str = learner_str, 
                           measure = measure, 
                           weight_space = weight_space)

save_tuning(measure_list = measure_list, 
            dataset_path = dataset_path, 
            dataset_imba_rate, 
            learner_str=learner_str, 
            measure = measure, 
            weight_space = weight_space)
