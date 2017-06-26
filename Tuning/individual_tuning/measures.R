library("data.table")

measureAUC = function(probabilities, truth, negative, positive) {
  
  print("length(probabilities)")
  print(length(probabilities))
  print("length(truth)")
  print(length(truth))
  
  if (is.factor(truth)) {
    i = as.integer(truth) == which(levels(truth) == positive)
  } else {
    i = truth == positive
  }
  
  if (length(unique(i)) < 2L) {
    stop("truth vector must have at least two classes")
  }
  #Use fast ranking function from data.table for larger vectors
  if (length(i) > 5000L) {
    r = frankv(probabilities)
  } else {
    r = rank(probabilities)
  }
  
  n.pos = as.numeric(sum(i))
  n.neg = length(i) - n.pos
  
  (sum(r[i]) - n.pos * (n.pos + 1) / 2) / (n.pos * n.neg)
}