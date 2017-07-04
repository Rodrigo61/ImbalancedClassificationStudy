
#' @param x matriz de atributos
#' @param y factor da variável dependente
#' @param learner.count, quantos modelos(interacoes) serao feitas

as.numeric.factor <- function(x) {as.numeric(as.character(x))}

ruspool <- function (task, learner, learner.count = 100, positive = 1, negative = 0, negative.fraction = -1) 
{
  
  models_pool = list()
  data     = getTaskData(task)
  n.negative = length(which(as.numeric.factor(data[,'y_data']) == negative))
  negative_index = !as.numeric.factor(data[,'y_data'])
  n <- nrow(data)
  indices <- 1:n
  
  # 50:50 by default
  if(negative.fraction == -1){
    n.positive = length(which(as.numeric.factor(data[,'y_data']) == positive))
    negative.fraction = n.positive/n.negative
  }
  
  for (i in 1:learner.count) {
    
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
  
  class(models_pool) <- "ruspool"
  models_pool
  
  
}


pred.ruspool = function(models_pool, new_data, threshold, positive = 1, negative = 0){
  
  # Vetor 
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


trainLearner.classif.ruspool = function(.learner, .task, .subset, .weights = NULL, learner.count, learner.name, ...) {
  learner = makeLearner("classif.ksvm")
  ruspool(task = subsetTask(.task, .subset), learner = learner, learner.count = learner.count)
}


predictLearner.classif.ruspool = function(.learner, .model, .newdata, threshold, ...) {
  
  threshold = 0.5
  p = pred.ruspool(models_pool = .model$learner.model, new_data = .newdata, threshold = threshold)
  
  if (.learner$predict.type == "response") 
    return(p$classif) else return(p$prob)
}


makeRLearner.classif.ruspool = function() {
  makeRLearnerClassif(
    cl = "classif.ruspool",
    package = "kernlab",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "learner.count", default = 40)
      #makeCharacterVectorParam(id = "learner.name", default="classif.ksvm")
    ),
    properties = c("twoclass", "numerics", "factors", "prob"),
    name = "Random Undersampling Pool",
    short.name = "ruspool",
    note = "Sem notas"
  )
}


registerS3method("makeRLearner", "classif.ruspool", makeRLearner.classif.ruspool)
registerS3method("trainLearner", "classif.ruspool", trainLearner.classif.ruspool)
registerS3method("predictLearner", "classif.ruspool", predictLearner.classif.ruspool)