#
# Script responsável por modelar o underbagging de acordo com os padroes S3 do MLR. 
# O underbagging nada mais é do que um ensemble de T classificadores de mesmo tipo 
# que são treinados em RUS do data set dado. Ou seja, dado um classificador do tipo C e
# um número T de classificadores desejados, então o underbagging irá realizar T
# treinamentos de classificadores do tipo C em cenário de RUS.
#
#
# Implementacao: O Script não está genérico, para adicionar tipos de classificadores deve-se
# seguir os seguintes passos:
#   -> Adicione como uma constante seu CL no inicio deste arquivo exemplo: "SVM_STR = "classif.ksvm""
#   -> Adicione na lista de parametros os parametros do seu novo classificador (funcao 'makeRLearner.classif.underbagging')
#   -> Adicione como parametros da funcao 'trainLearner.classif.underbagging' os parametros adicionados acima
#   -> Crie um fluxo adicional para seu classificador na funcao 'trainLearner.classif.underbagging'

SVM_STR = "classif.ksvm"
RF_STR = "classif.randomForest"
XGBOOST_STR = "classif.xgboost"
RPART_STR = "classif.rpart"
KNN_STR = "classif.kknn"

#' @param x matriz de atributos
#' @param y factor da variável dependente
#' @param learner_count, quantos modelos(interacoes) serao feitas

as.numeric.factor <- function(x) {as.numeric(as.character(x))}

underbagging <- function (data, learner, learner_count = 100, positive = 1, negative = 0, negative.fraction = -1) 
{
  
  models_pool = list()
  n.negative = length(which(as.numeric.factor(data[,'y_data']) == negative))
  negative_index = !as.numeric.factor(data[,'y_data'])
  n <- nrow(data)
  indices <- 1:n
  
  # 50:50 by default
  if(negative.fraction == -1){
    n.positive = length(which(as.numeric.factor(data[,'y_data']) == positive))
    negative.fraction = n.positive/n.negative
  }
  
  for (i in 1:learner_count) {
    
    # create a subset index from the 1:n vector, using ALL "Y"s and a SAMPLE of "N"s
    subset.index = c(
      sample(indices[negative_index], n.negative*negative.fraction, replace = FALSE),  
      indices[!negative_index])
    
    # this is where the sample is subset in each iteration
    tmp.sample = data[subset.index,]
    
    # Treinando o modelo
    model = mlr::train(learner, makeClassifTask(data=tmp.sample, target='y_data', positive=positive))

    # Adiciona o modelo ao pool
    models_pool[[i]] = model
  }
  
  class(models_pool) <- "underbagging"
  models_pool
  
  
}


predict.underbagging = function(models_pool, new_data, threshold, positive = 1, negative = 0){
  
  data_probs = NULL
  
  for (i in 1:nrow(new_data)) {
    obs = new_data[i,]
    # Recolhendo os votos de cada modelo do pool
    votes = vector()
    for(model in models_pool){
      pred = predict(model, newdata = obs)
      t = as.numeric.factor(getPredictionResponse(pred))
      votes = c(votes, t)
    }
    
    n_pos = length(which(votes == positive))
    n_neg = length(which(votes == negative))
    
    prob_positive = n_pos / (n_neg + n_pos)
    
    data_probs = c(data_probs, prob_positive)
    
  }
  
  # Formatando a saida binaria
  binary_classif = data_probs
  binary_classif[which(binary_classif >= threshold)] = positive
  binary_classif[which(binary_classif < threshold)] = negative
  binary_classif = as.factor(binary_classif)
  
  # Formatando a saida de probabilidades
  pos_probs = data_probs
  neg_probs = 1 - pos_probs
  data_probs = matrix(c(pos_probs, neg_probs), nrow=length(pos_probs)) # TODO: rever ordem das colunas
  colnames(data_probs) <- c(as.character(positive), as.character(negative))
  
  ans <- list(classif = binary_classif, prob = data_probs)
  ans
  
}


################################################################
################## MLR S3 CLASS ################################
################################################################

trainLearner.classif.underbagging = function(.learner, .task, .subset, .weights = NULL, 
                                        learner_count, learner_name, C, sigma, mtry, ntree, max_depth, eta, nrounds, xval, k, ...) {
  
  learner = makeLearner(learner_name)
  
  #Selecionando os h.p especificos
  pars = list()
  if(learner_name == SVM_STR){
    pars$C = C
    pars$sigma = sigma
  }else if(learner_name == RF_STR){
    pars$mtry = mtry
    pars$ntree = ntree
  }else if(learner_name == XGBOOST_STR){
    pars$max_depth = max_depth
    pars$eta = eta
    pars$nrounds = nrounds
  }else if(learner_name == RPART_STR){
    pars$xval = xval
  }else if(learner_name == XGBOOST_STR){
    pars$k = k
  }else{
    warning(paste("Nao conheco os hiperparametros para o learner_name = ", learner_name, sep=""))
    stop()
  }
  
  learner = setHyperPars(learner, par.vals = pars)    
  data = getTaskData(subsetTask(.task, .subset))
  underbagging(data = data, learner = learner, learner_count = learner_count)
}


predictLearner.classif.underbagging = function(.learner, .model, .newdata, ...) {
  
  threshold = 0.5
  p = predict(models_pool = .model$learner.model, new_data = .newdata, threshold = threshold)
  
  if (.learner$predict.type == "response") 
    return(p$classif) else return(p$prob)
}


makeRLearner.classif.underbagging = function() {
  makeRLearnerClassif(
    cl = "classif.underbagging",
    package = "mlr",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "learner_count", default = 40),
      makeDiscreteLearnerParam(id = "learner_name", default = SVM_STR,
                               values = c(RF_STR, XGBOOST_STR, SVM_STR)),
      # SVM params
      makeNumericLearnerParam(id = "C"),
      makeNumericLearnerParam(id = "sigma"),
      # RF Params
      makeIntegerLearnerParam(id = "mtry"),
      makeIntegerLearnerParam(id = "ntree"),
      # XGBoost Params
      makeIntegerLearnerParam(id = "max_depth"),
      makeNumericLearnerParam(id = "eta"),
      makeIntegerLearnerParam(id = "nrounds", default = 1L, lower = 1L),
      # Rpart Params
      makeIntegerLearnerParam(id = "xval", default = 0),
      # knn Params
      makeIntegerLearnerParam(id = "k", default = 1)
    ),
    properties = c("twoclass", "numerics", "factors", "prob"),
    name = "Random Undersampling Bagging",
    short.name = "underbagging",
    note = "Sem notas"
  )
}


registerS3method("makeRLearner", "classif.underbagging", makeRLearner.classif.underbagging)
registerS3method("trainLearner", "classif.underbagging", trainLearner.classif.underbagging)
registerS3method("predictLearner", "classif.underbagging", predictLearner.classif.underbagging)