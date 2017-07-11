##
# Script responsável por calcular a melhor performance a respeito
# das métricas para cada algoritmo desejado. É realizado um CV k-fold e um randomSearch para
# hiperparametros. Alguns parametros sao setados ao inicio do 
# script. O script deve ser extensivel e totalmente independente, de modo que o tuning
# eh feito apenas para uma das possiveis combinacoes de metrica X algoritmo X cenario por vez.
#
#
# Veja o README para informacoes de como adicionar novas combinacoes de tuning.
##


library(mlr)
library(stringr)
library(xgboost)
library(caret)
library(optparse)
library(smotefamily)
#library(rusboost)
library(rpart)
set.seed(3)
source("../RUSBoost.R")
source("../RUSPool.R")
source("../measures.R")

#**************************************************************#
#*******************  CONSTANTES   ****************************#
#**************************************************************#

#Quantas iteracoes serao feitas no random search
MAX_IT = 10L
#parametro K do K-folds
ITERS = 3L
DEBUG = T
SVM_STR = "classif.ksvm"
RF_STR = "classif.randomForest"
XGBOOST_STR = "classif.xgboost" 
RUSPOOL_STR = "classif.ruspool"
RUSBOOST_STR = "classif.rusboost"
SUMMARY_FOLDER_NAME = "summary_files"
DATASET_LIST_PATH = "../dataset_list_RECOD"
#DATASET_LIST_PATH = "../dataset_list"
COLUMNS_NAMES = c("learner", "weight_space", "measure", "sampling",
                  "tuning_measure", "holdout_measure", 
                  "holdout_measure_residual", "iteration_count")

POSITIVE_CLASS = "1"
NEGATIVE_CLASS = "0"

SMOTE_STR = "SMOTE"
ADASYN_STR = "ADASYN"
UNDERBAGGING_STR = "UNDERBAGGING"


#**************************************************************#
#*******************  VAR. GLOBAIS ****************************#
#**************************************************************#
c.dataset = NULL
c.dataset_path = NULL
c.residual_dataset = NULL
c.residual_dataset_path = NULL
c.learner_str = NULL
c.measure = NULL
c.weight_space = FALSE
c.oversampling_method = FALSE
c.ruspool = FALSE

#**************************************************************#
#*******************  FUNCOES     ******************************
#**************************************************************#

#----------------------#
c.print_debug = function(str){
  if(!is.character(str)){
    str = paste(str, collapse = "")
  }
  if(DEBUG){
    print(paste("[DEBUG]", str, sep=""))
  }
}
#----------------------#

c.get_args = function(){
  description = " Script responsável por calcular a melhor performance a respeito
  das métricas para cada algoritmo desejado. É realizado um CV k-fold e um randomSearch para
  hiperparametros. Alguns parametros sao setados ao inicio do 
  script. O script deve ser extensivel e totalmente independente, de modo que o tuning
  eh feito apenas para uma das possiveis combinacoes de metrica X algoritmo X cenario por vez"
  
  option_list = list(
    make_option(c("--dataset_id"), type="integer", default=NULL, 
                help="ID do dataset a ser utilizado"),
    
    make_option(c("--measure"), type="character", default=NULL, 
                help="nome da métrica utilizada para otimizacao"),
    
    make_option(c("--model"), type="character", default=NULL, 
                help="nome do algoritmo que será realizado o tuning"),
    
    make_option(c("--weight_space"), action= "store_true", default=NULL, 
                help="se presente a flag o treinamento será feito com weight_space"),
    
    make_option(c("--oversampling"), type= "character", default=NULL, 
                help="nome do algoritmo de oversampling que será utilizado no dataset corrente"),
    
    make_option(c("--ruspool"), action= "store_true", default=NULL, 
                help="se presente a flag o modelo será encapsulado por um ensamble RUSPool")
    
  )
  
  opt_parser = OptionParser(option_list=option_list, description = description)
  return(parse_args(opt_parser))
}

#----------------------#

c.create_holdout_train_test = function(k){
  #Funcao que centraliza o calculo para separacao de k-1/k treino 1/k test do holdout. Essa distruibuicao
  # é aleatoria.
  
  # Embaralhando dataset de forma aleatoria
  c.dataset = c.dataset[sample(nrow(c.dataset)), ]
  
  # Definindo os indices que serao destinados para o test. Isso é feito com uma simples divisao 
  # de quantas observacoes deveriam ir para cada um dos 5 folds. Lembrando que o teste corresponde
  # a 1 dos 5 folds. Nao era possivel usar o createFolds do caret, pois o mesmo nao garantia estratificacao
  # com datasets com raridade absoluta.
  positive_indexes = which(c.dataset[, 'y_data'] == 1)
  positive_count = length(positive_indexes)
  positive_count_for_test = floor(positive_count/k)
  positive_indexes_for_test = positive_indexes[1:positive_count_for_test]
  
  negative_indexes = which(c.dataset[, 'y_data'] == 0)
  negative_count = length(negative_indexes)
  negative_count_for_test = floor(negative_count/k)
  negative_indexes_for_test = negative_indexes[1:negative_count_for_test]
  
  holdout_sets = NULL
  holdout_sets$train = c.dataset[-c(positive_indexes_for_test, negative_indexes_for_test), ]
  holdout_sets$test = c.dataset[c(positive_indexes_for_test, negative_indexes_for_test), ]
  
  return(holdout_sets)
}


#----------------------#
#Funcao que retorna o custo da classe majoritaria para o class_weight learning
c.get_majority_weight = function(){
  if(c.weight_space == T){
    #Definimos essa razao como o custo de erro da classe majoritária.
    MAJORITY_weight = length(which(c.dataset[, 'y_data'] == 1))/length(which(c.dataset[, 'y_data'] == 0))  
  }else{
    MAJORITY_weight = 1 #remove a influencia do cost learn
  }
}

#----------------------#
#Funcao que criar um learner e já empacota opcoes de h.parametro e class_weight 
c.makeLearnerWrapped = function(hiper.par.vals = NULL){

  learner = makeLearner(c.learner_str)  
  
  # Fazendo um wrapper para Weighted classes. Lembrar que 0 nos ds estudados a classe majoritaria vem antes
  if(c.weight_space == TRUE){
    majority_weight = c.get_majority_weight()
    learner = makeWeightedClassesWrapper(learner, wcw.weight = majority_weight) 
  }
  
  if(!is.null(hiper.par.vals)){
    learner = setHyperPars(learner, par.vals = hiper.par.vals)    
  }
  
  if(identical(c.measure, auc)){
    #to calculate AUC we need some continuous output, so we set 
    #predictType to probabilities
    learner = setPredictType(learner, "prob")
  }
  
  return(learner)
}

#----------------------#
c.get_measures_from_tuneParams = function(search_space, train, test){
  
  #Definindo configuracoes pro CV(k_fold) do tuning
  ctrl = makeTuneControlRandom(maxit = MAX_IT)
  rdesc = makeResampleDesc("CV", iters = ITERS, stratify = TRUE)
  
  #Definindo variavel de retorno da funcao
  result = NULL
  
  #Aplica oversampling caso seja passado como parametro do script, ele é realizado apenas no conjunto de treino
  if(c.oversampling_method == TRUE){
    train = c.exec_data_preprocessing(train)  
  }
  
  # Caso tenha a opcao de RUSPool, encapsulamos o learner escolhido 
  if(c.ruspool == TRUE){
    ruspool_params = makeParamSet(
      makeDiscreteParam("learner_count", c(10,20,30,40,50,60)),
      makeDiscreteParam("learner_name", c.learner_str)
    )
    search_space = makeParamSet(params = c(search_space$pars, ruspool_params$pars))
    learner_aux = c.learner_str
    c.learner_str <<- RUSPOOL_STR
  }
  
  learner = c.makeLearnerWrapped() 
  
  res_tuneParams = tuneParams(learner, 
                              task = makeClassifTask(data=train, target='y_data', positive=POSITIVE_CLASS), 
                              resampling = rdesc,
                              par.set = search_space, 
                              control = ctrl, 
                              measure=c.measure, 
                              show.info = DEBUG)    
  
  
  #Armazenando melhor resultado obtido internamente no tuning
  result$performance_tuned = res_tuneParams$y
  
  learner = c.makeLearnerWrapped(hiper.par.vals =res_tuneParams$x)

  #Obtendo e armazenando o resultado do holdout com os hp. obtidos pelo tuning
  #Holdout normal
  learner_res = mlr::train(learner, makeClassifTask(data=train, target='y_data', positive=POSITIVE_CLASS))
  p = predict(learner_res, task = makeClassifTask(data=test, target='y_data', positive=POSITIVE_CLASS))
  result$performance_holdout = performance(p, measures = c.measure)
  
  #Holdout com conjunto de teste extendido com os residuos do dataset
  test = rbind(test, c.residual_dataset)
  p = predict(learner_res, task = makeClassifTask(data=test, target='y_data', positive=POSITIVE_CLASS))
  result$performance_holdout_with_residual = performance(p, measures = c.measure)
  
  
  # TODO: Melhorar esse fluxo confuso.
  # Caso tenha a opcao de RUSPool, desencapsulamos o learner escolhido 
  if(c.ruspool == TRUE){
    c.learner_str <<- learner_aux
  }
  
  return(result)
}


#----------------------#
c.gen_all_measures_inline = function(){
  
  measures_compilation = vector("list", ITERS)

  #Repetimos 3x a busca pelas performances
  for (i in 1:ITERS){
    
    #Seperando treino e teste (Holdout estratificado). 80%(4/5) dos dados para treino e 
    #o restante para teste.
    holdout_aux = c.create_holdout_train_test(5);
    train = holdout_aux$train
    test = holdout_aux$test
    
    
    #Realizando Tuning com o search_space correspondente
    search_space = c.select_search_space()
    measures = c.get_measures_from_tuneParams(search_space, train, test)  
    
    
    #Adicionando todas as colunas do df final
    new_row = c(c.learner_str, 
                c.weight_space, 
                c.measure$name,
                c.oversampling_method,
                measures$performance_tuned, 
                measures$performance_holdout, 
                measures$performance_holdout_with_residual, 
                i)
    
    measures_compilation[[i]] = new_row
    
  }
  return(measures_compilation)
}

#----------------------#
c.select_measure = function(arg){
  if(is.na(arg) | is.null(arg)){
    warning("Nao foi informado a metrica desejada")
    stop()
  }
  
  if(arg == "acc"){
    return(acc)
  }else if(arg == "f1"){
    return(f1)
  }else if(arg == "gmeans"){
    return(gmean)   # Ref [1]
  }else if(arg == "auc"){
    return(auc)
  }else if(arg == "mcc"){
    return(mcc)
  }else{
    warning("Selecione uma das seguintes metricas: acc, f1, gmeans, mcc, auc")
    stop()
  }
}

#----------------------#
c.select_learner = function(arg){
  if(is.null(arg) || is.na(arg)){
    warning("Nao foi informado o algoritmo desejado")
    stop()
  }
  
  if(arg == "svm"){
    return(SVM_STR) # Por default o SVM é RBF (Ref [2])
  }else if(arg == "rf"){
    return(RF_STR)
  }else if(arg == "xgboost"){
    return(XGBOOST_STR)
  }else if(arg == "rusboost"){
    return(RUSBOOST_STR)
  }else{
    warning("Selecione um dos seguintes algoritmos: svm, rf, xgboost, rusboost")
    stop()
  }
}

#----------------------#
c.select_weight_space = function(arg){
  if(is.null(arg)){
    return(F)
  }else{
    return(T)
  }
}

#----------------------#
c.select_oversampling = function(arg){
  
  #Caso a flag nao tenha sido passada retorna NULL
  if(is.null(arg) || is.na(arg)){
    return(FALSE)
  }
  
  if(arg == "smote"){
    return(SMOTE_STR) 
  }else if(arg == "adasyn"){
    return(ADASYN_STR)
  }else{
    warning("Selecione um algoritmo de oversampling válido: smote, adasyn")
    stop()
  }
}

#----------------------#
c.select_ruspool = function(arg){
  if(is.null(arg)){
    return(F)
  }else{
    return(T)
  }
}

#----------------------#
c.validate_params = function(){
  # Essa funcao apenas interrompe o programa caso uma combinacao indesejada seja feita nos parametros de ajuste
  
  if(c.weight_space == T){
    if(!is.null(c.oversampling_method)){
      warning("Atualmente o script está impossibilitado de realizar OVERSAMPLING + WEIGHT SPACE")
      stop()
    }
    if(c.ruspool == T){
      warning("Atualmente o script está impossibilitado de realizar RUSPOOL + WEIGHT SPACE")
      stop()
    }
    if(c.learner_str == RUSBOOST_STR){
      warning("Atualmente o script está impossibilitado de realizar RUSBoost + WEIGHT SPACE")
      stop()
    }
  }
}
#----------------------#
c.select_search_space = function(){
  if(c.learner_str == SVM_STR){
    return(
      makeParamSet(
        makeNumericParam("C", lower = 2**(-5), upper = 2**15),
        makeNumericParam("sigma", lower = 2**(-15), upper = 2**3)
      )
    )
  }else if(c.learner_str == RF_STR){
    return(
      makeParamSet(
        makeDiscreteParam("mtry", c(1:(ncol(c.dataset)-1))),
        makeDiscreteParam("ntree", c((2**4):(2**12)))
      )
    )
  }else if(c.learner_str == XGBOOST_STR){
    return(
      makeParamSet(
        makeDiscreteParam("max_depth", c(1:6)),
        makeNumericParam("eta", lower=0.005, upper = 0.5),
        makeDiscreteParam("nrounds", seq(20, 150, 20))
      )
    )
  }else if(c.learner_str == RUSBOOST_STR){
    return(
      makeParamSet(
        makeDiscreteParam("learner.count", c(10,20,30,40,50,60))
      )
    )
  }else{
    warning(paste("Nao existe um search_space definido para o algoritmo ", c.learner_str, sep=""))
    stop()
  }
}

#----------------------#
c.exec_tuning = function(){
  
  tuning_and_holdout = c.gen_all_measures_inline()
  
  # Prints de debug
  c.print_debug("Resultados do tuning:")
  c.print_debug(paste(COLUMNS_NAMES, collapse=" | "))
  print(tuning_and_holdout)
  
  return(tuning_and_holdout)
}

#----------------------#
#Funcao que salva a lista de medidas obtidas pelo tuning em arquivos com nomes adequados ao dataset
c.save_tuning = function(measure_list){
  
  #compilando a lista de metricas em um unico dataframe
  out_df = NULL
  for(i in 1:ITERS){
    out_df = rbind(out_df, measure_list[[i]])
  }
  
  colnames(out_df) = COLUMNS_NAMES
  
  #Criando caso nao exista a pasta para salvar os arquivos com os resultados
  dirname = paste(SUMMARY_FOLDER_NAME, as.character(c.dataset_imba_rate), sep="")
  dir.create(file.path(dirname(c.dataset_path), dirname), showWarnings = DEBUG)
  
  #Salvando dados
  out_filename = paste(c.learner_str, 
                       c.measure$name, 
                       as.character(c.weight_space), 
                       as.character(c.oversampling_method), 
                       sep ="_")
  out_path = str_replace_all(paste(dirname(c.dataset_path), paste(dirname, out_filename, sep="/"), sep="/"), " ", "_")
  write.table(out_df, out_path, col.names = T, row.names = F, sep=",")
  
  c.print_debug("Resultados obtidos")
  print(out_df)
  
  c.print_debug(paste("Tuning salvo em: ", out_path, sep=""))
}

#----------------------#
#Funcao que executa em c.dataset o algoritmo de sampling escolhido pelo usuario
c.exec_data_preprocessing = function(ds){
  
  sampled_dataset = NULL
  dataset_features = ds[ ,-ncol(ds)]
  dataset_classes = ds[ ,ncol(ds)]
  
  if(c.oversampling_method == ADASYN_STR){
    
    sampled_dataset = ADAS(dataset_features, dataset_classes)$data
    
  }else if(c.oversampling_method == SMOTE_STR){
    
    sampled_dataset = SMOTE(dataset_features, dataset_classes)$data
    
  }else{
    c.print_debug("Houve um erro interno! a variavel oversampling_method nao está com um valor correto!")
    return()
  }
  
  # Seta a coluna das classes com o nome anterior: 'y_data'
  colnames(sampled_dataset) = c(colnames(sampled_dataset)[-(length(colnames(sampled_dataset)))], 'y_data')
  
  # Log para o usuario
  c.print_debug(paste("Executado ", c.oversampling_method, sep=""))
  c.print_debug(paste("Original dataset majority number = ", length(which(ds[, 'y_data'] == 0)), sep=""))
  c.print_debug(paste("Original dataset majority number = ", length(which(ds[, 'y_data'] == 1)), sep=""))
  c.print_debug(paste("Sampled dataset majority number = ", length(which(sampled_dataset[, 'y_data'] == 0)), sep=""))
  c.print_debug(paste("Sampled dataset minority number = ", length(which(sampled_dataset[, 'y_data'] == 1)), sep=""))
  
  return(sampled_dataset)
  
}

#**************************************************************#
#*******************  MAIN   **********************************#
#**************************************************************#

#Lendo os parametros o script
opt = c.get_args()

#Lendo lista dos datasets
dataset_list = read.csv(DATASET_LIST_PATH, header=F)

#Selecionando dataset pela posicao na lista
dataset_id = as.numeric(opt$dataset_id) + 1
c.dataset_path = as.character(dataset_list[dataset_id,])
dataset_dir = dirname(c.dataset_path)
c.dataset_imba_rate = str_extract(c.dataset_path, "0.[0-9]{2,3}")

#Selecionando os parametros para o tuning
c.measure = c.select_measure(opt$measure)
c.learner_str = c.select_learner(opt$model)
c.weight_space = c.select_weight_space(opt$weight_space)
c.oversampling_method = c.select_oversampling(opt$oversampling)
c.ruspool = c.select_ruspool(opt$ruspool)

# Validando parametros para o tuning
c.validate_params()

#Carregando dataset
c.dataset = read.csv(c.dataset_path, header = T)

# Coluna dependente deve ser factor para melhor funcionamento das bibliotecas
c.dataset[, "y_data"] = as.factor(c.dataset[, "y_data"])

#Carregando o resíduo do dataset
c.residual_dataset_path = paste(dirname(c.dataset_path),"/residual_", c.dataset_imba_rate, ".csv", sep="")
c.residual_dataset = read.csv(c.residual_dataset_path, header = T)

#Executando e armazenando os valores obtidos com o tuning
c.print_debug("Executando o tuning com os seguintes parametros:")
c.print_debug(paste("Dataset: ", c.dataset_path))
c.print_debug(paste("Algoritmo: ", c.learner_str))
c.print_debug(paste("Metrica: ", c.measure$name))
c.print_debug(paste("Weitgh space: ", c.weight_space))
c.print_debug(paste("Oversampling method: ", c.oversampling_method))
c.print_debug(paste("RUSPool: ", c.ruspool))

#Executando e obtendo os resultados para o tuning com os parametros dados
measure_list = c.exec_tuning()

#Salvando os dados obtidos dos tuning
c.save_tuning(measure_list = measure_list)


##
## REFERENCIAS
##

# [1] R. Barandela, J.S. Sánchez, V. García, E. Rangel, Strategies for learning in class imbalance problems, Pattern Recognition 36 (3) (2003) 849–851
# [2] https://github.com/mlr-org/mlr/blob/master/R/RLearner_classif_ksvm.R
