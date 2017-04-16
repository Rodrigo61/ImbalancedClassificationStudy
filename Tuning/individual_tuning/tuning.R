##
# Script responsável por calcular a melhor performance a respeito
# das métricas para cada algoritmo desejado. É realizado um CV k-fold e um randomSearch para
# hiperparametros. Alguns parametros sao setados ao inicio do 
# script. O script deve ser extensivel e totalmente independente, de modo que o tuning
# eh feito apenas para uma das possiveis combinacoes de metrica X algoritmo X cenario por vez.
#
##


library(mlr)
library(stringr)
library(xgboost)
library(caret)
library(optparse)
set.seed(3)

#**************************************************************#
#*******************  CONSTANTES   ****************************#
#**************************************************************#

#Quantas iteracoes serao feitas no random search
MAX_IT = 20L
#parametro K do K-folds
ITERS = 3L
DEBUG = T
SVM_STR = "classif.ksvm"
RF_STR = "classif.randomForest"
XGBOOST_STR = "classif.xgboost" 
SUMMARY_FOLDER_NAME = "summary_files"
DATASET_LIST_PATH = "../dataset_list_RECOD"
#DATASET_LIST_PATH = "../dataset_list"
COLUMNS_NAMES = c("learner", "weight_space", "measure",
                  "tuning_measure", "holdout_measure",
                  "iteration_count")

NEGATIVE_CLASS = "0"
POSITIVE_CLASS = "1"

SMOTE_STR = "SMOTE"
SMOTE_BORDERLIN_STR = "SMOTE_BORDERLINE"
ADASYN_STR = "ADASYN"


#**************************************************************#
#*******************  VAR. GLOBAIS ****************************#
#**************************************************************#
c.dataset = NULL
c.dataset_path = NULL
c.learner_str = NULL
c.measure = NULL
c.weight_space = FALSE
c.oversampling_method = FALSE

#**************************************************************#
#*******************  FUNCOES     ******************************
#**************************************************************#

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
    make_option(c("--oversampling"), action= "character", default=NULL, 
                help="nome do algoritmo de oversampling que será utilizado no dataset corrente")
    
  )
  
  opt_parser = OptionParser(option_list=option_list, description = description)
  return(parse_args(opt_parser))
}

#----------------------#
#Funcao que centraliza o calculo para separacao de 4/5 treino 1/5 test do holdout
c.create_holdout_train_test = function(){
  
  folds = createFolds(c.dataset[, 'y_data'], 5);
  train = c.dataset[c(folds$Fold1, folds$Fold2, folds$Fold3, folds$Fold4),]
  test = c.dataset[folds$Fold5,]
  
  holdout_sets = NULL
  holdout_sets$train = train
  holdout_sets$test = test
  
  return(holdout_sets)
}

#----------------------#
#Funcao que retorna o custo da classe majoritaria para o class_weight learning
c.get_majority_weight = function(){
  if(c.weight_space == T){
    #Definimos essa razao como o custo de erro da classe majoritária.
    MAJORITY_weight = length(which(c.dataset['y_data'] == 1))/length(which(c.dataset['y_data'] == 0))  
  }else{
    MAJORITY_weight = 1 #remove a influencia do cost learn
  }
}

#----------------------#
#Funcao que criar um learner e já empacota opcoes de parametro e class_weight 
c.makeLearnerWrapped = function(par.vals, hiper.par.vals){
  
  #Defidindo peso da classe majoritaria. Se o weight_space for False esse peso nao vai interferir em nada
  majority_weight = c.get_majority_weight()
  
  learner = makeLearner(c.learner_str, par.vals = par.vals)
  learner = makeWeightedClassesWrapper(learner, wcw.param = NEGATIVE_CLASS, wcw.weight = majority_weight)
  learner = setHyperPars(learner, par.vals = hiper.par.vals)  
  
  if(identical(measure, auc)){
    #to calculate AUC we need some continuous output, so we set 
    #predictType to probabilities
    learner = setPredictType(learner, "prob")
  }
  
  return(learner)
}

#----------------------#
c.get_measures_from_tuneParams = function(){
  #AUX do xgboost
  best_nrounds = 20
  best_measure = 0
  
  
  #Definindo configuracoes pro CV(k_fold)
  ctrl = makeTuneControlRandom(maxit = MAX_IT)
  rdesc = makeResampleDesc("CV", iters = ITERS)
  
  #Definindo variavel de retorno da funcao
  result = NULL

  #Seperando treino e teste (Holdout estratificado). 80%(4/5) dos dados para treino e 
  #o restante para teste.
  holdout_aux = c.create_holdout_train_test(c.dataset = c.dataset[, 'y_data']);
  train = holdout_aux$train
  test = holdout_aux$test
  
  
  
  #Realizando o tuning com a métrica escolhida
  if(c.learner_str == XGBOOST_STR){

    #O parametro nrounds não é comtemplado pelo search_space do XGboost no MLR. De modo que devemos realiza-lo
    # de fora do tuning manualmente.
    for(nrounds in seq(20, 150, 20)){  

        learner = c.makeLearnerWrapped(par.vals = list(nrounds = nrounds))
        res_tuneParams = tuneParams(learner, 
                                    task = makeClassifTask(data=train, target='y_data', positive=POSITIVE_CLASS), 
                                    resampling = rdesc, 
                                    par.set = search_space, 
                                    control = ctrl, 
                                    measure=measure, 
                                    show.info = DEBUG)    

        if(res_tuneParams$y > best_measure){
          best_nrounds = nrounds
          best_measure = res_tuneParams$y
        }

    }
    c.print_debug("BEST NROUNDS")
    c.print_debug(best_nrounds)
    
  }else{
    # Todos os outros algoritmos tem seus hiperparametros corretamente listado no seach_space.
    res_tuneParams = tuneParams(c.learner_str, 
                                task = makeClassifTask(data=train, target='y_data', positive=POSITIVE_CLASS), 
                                resampling = rdesc,
                                par.set = search_space, 
                                control = ctrl, 
                                measure=c.measure, 
                                show.info = DEBUG)    
  }

  #Armazenando melhor resultado obtido internamente no tuning
  result$performance_tuned = res_tuneParams$y
  
  #Treinando um modelo com o treino e os hiperparametros obtidos e armazenando a performance
  if(c.learner_str == XGBOOST_STR){
    #Novamente temos que escrever codigo adicional para adicionar o hiperparametro nrounds ao xgboost
    learner = c.makeLearnerWrapped(hiper.par.vals =res_tuneParams$x, par.vals = list(nrounds = nrounds))
  }else{
    learner = c.makeLearnerWrapped(hiper.par.vals =res_tuneParams$x)
  }
  
  #Obtendo e armazenando o resultado do holdout com os hp. obtidos pelo tuning
  learner_res = mlr::train(learner, makeClassifTask(data=train, target='y_data', positive=POSITIVE_CLASS))
  p = predict(learner_res, task = makeClassifTask(data=test, target='y_data', positive=POSITIVE_CLASS))
  result$performance_holdout = performance(p, measures = c.measure)

  return(result)
}

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
c.gen_all_measures_inline = function(){
  
  measures_compilation = vector("list", ITERS)
  #Repetimos 3x a busca pelas performances
  for (i in 1:ITERS){
    
    #Realizando Tuning com métrica acurácia
    measures = c.get_measures_from_tuneParams(search_space = search_space, 
                                            dataset = c.dataset, 
                                            learner_str = c.learner_str, 
                                            measure = c.measure, 
                                            weight_space = c.weight_space)
 
    new_row = c(c.learner_str, c.weight_space, c.measure$name, measures$performance_tuned, 
                measures$performance_holdout, i)
    
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
    return(gmean)
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
    warning("Selecione um dos seguintes algoritmos: svm, rf, xgboost")
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
  if(arg == "smote"){
    return(SMOTE_STR) 
  }else if(arg == "adasyn"){
    return(ADASYN_STR)
  }else if(arg == "borderline"){
    return(SMOTE_BORDERLINE_STR)
  }else{
    warning("Selecione um dos seguintes algoritmos: smote, adasyn ou borderline")
    stop()
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
        makeNumericParam("eta", lower=0.005, upper = 0.5)
      )
    )
  }else{
    warning(paste("Nao existe um search_space definido para o algoritmo ", c.learner_str, sep=""))
    stop()
  }
}

#----------------------#
c.exec_tuning = function(){
  
  search_space = c.select_search_space()
  
  tuning_and_holdout = c.gen_all_measures_inline(search_space = search_space, 
                                               dataset = c.dataset,
                                               learner_str = c.learner_str,
                                               weight_space = c.weight_space,
                                               measure = c.measure)
  
  c.print_debug("Resultados do tuning:")
  c.print_debug(paste(COLUMNS_NAMES, collapse=" | "))
  print(tuning_and_holdout)
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
  out_filename = paste(c.learner_str, measure$name, as.character(c.weight_space), sep ="_")
  out_path = str_replace_all(paste(dirname(c.dataset_path), paste(dirname, out_filename, sep="/"), sep="/"), " ", "_")
  write.table(out_df, out_path, col.names = T, row.names = F, sep=",")
  
  c.print_debug(paste("Tuning salvo em: ", out_path, sep=""))
}

c.exec_data_preprocessing = function(){
  if(c.oversampling_method == ADASYN_STR){
    print("Verificando ADAS(c.dataset[,-'y_data'], c.dataset[,'y_data'])$data")
    print(ADAS(c.dataset[,-'y_data'], c.dataset[,'y_data'])$data)
    return(ADAS(c.dataset[,-'y_data'], c.dataset[,'y_data'])$data)
  }else{
    c.print_debug("Houve um erro interno! a variavel oversampling_method nao está com um valor correto!")
  }
  
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

#Carregando dataset
c.dataset = read.csv(c.dataset_path, header = T)

#Aplicando data pre-processing, se pedida pelo usuario
c.dataset = c.exec_data_preprocessing()

#Executando e armazenando os valores obtidos com o tuning
c.print_debug("Executando o tuning com os seguintes parametros:")
c.print_debug(paste("Dataset: ", c.dataset_path))
c.print_debug(paste("Algoritmo: ", c.learner_str))
c.print_debug(paste("Metrica: ", c.measure$name))
c.print_debug(paste("Weitgh space: ", c.weight_space))
c.print_debug(paste("Oversampling method: ", c.oversampling_method))

#Executando e obtendo os resultados para o tuning com os parametros dados
measure_list = c.exec_tuning()

#Salvando os dados obtidos dos tuning
c.save_tuning(measure_list = measure_list)
