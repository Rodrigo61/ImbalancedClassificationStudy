# Este módulo é responsável por preencher com linhas vazias as informacoes faltantes em cada summary_file 
# dos data sets artificialmente desbalanceados. Ou seja, se um data set desbalanceado não teve alguma informacao 
# computada que deveria ter sido, entre todas as combinacoes de cenários esperadas, esse módulo deve preencher 
# esse cenarios com NA's de modo que todos os summary_files contenham a mesma quantidade de linhas. Por fim essas 
# linhas são ordenadas para que possamos comparar lado a lado quaisquer dois summary_files.
#
#
# Verifique o README para informacoes sobre como adicionar novas verificacoes de colunas


library(stringr)
library(Jmisc)

#**************************************************************#
#*******************  FUNCOES   *******************************#
#**************************************************************#

fix_missing_combination = function(summary){
  # Verifica e resolve todas as técnicas utilizadas no estudo. IMPORTANTE: Supomos para esse métodos que não há mistura de técnicas.
  
  measures = c('Accuracy', 'Area under the curve', "F1 measure", "G-mean", "Matthews correlation coefficient")
  learners = c('classif.ksvm', 'classif.randomForest', 'classif.xgboost')
  techniques = list()
  techniques$sampling = c('SMOTE', 'ADASYN')
  techniques$ruspool = c('TRUE')
  techniques$weight_space = c('TRUE')
  
  #Vamos fazer a busca para todas as combinacoes nesse summary
  for (technique in names(techniques)) {
    
    technique_options = techniques[[techinique]]
    for (option in technique_options){
      for(learner in learners){
        for(measure in measures){
          
          combination_count = lenght(which(summary[, 'learner'] == learner 
                                           & summary[, 'measure'] == measure 
                                           & summary[, technique] == option))
          
          # Nao existe medicao para essa combinacao, devemos gerar 3 linhas vazias entao
          if(combination_count == 0){
            empty_line = c(learner, F, measure, F, F, NA, NA, NA, NA)
            summary = rbind(summary, empty_line)
            summary = rbind(summary, empty_line)
            summary = rbind(summary, empty_line)
            
            # Inserimos todas as tecnicas como falsa e só atualizamos a tecnica da combinacao atual
            # isso só é possível pela suposicao de que as técnicas nao se misturam
            summary[(NROW(summary)-3):NROW(summary), technique] = option
          }
          
        }
      }
    }
    
  }
  
  return(summary)
}


#**************************************************************#
#*******************  MAIN      *******************************#
#**************************************************************#

SUMMARY_LIST_FILENAME = "summary_list"

summary_list = as.vector(read.csv(SUMMARY_LIST_FILENAME, header = F))

for(summary_file_name in summary_list[,1]){
  
  print(paste("Lendo o arquivo: ", summary_file_name, sep=''))
  summary_file_name = as.character(summary_file_name)
  summary = read.csv(summary_file_name, header = T)
  
  # Verifica todas as combinacoes que deveriam ter medicoes, mas nao tem e as completa com NA's
  summary = fix_missing_combination(summary)
  
  #Ordenando pelas colunas o summary
  summary = summary[do.call(order, lapply(1:NCOL(summary), function(i) summary[, i])), ]
  
  #Salvando summary atualizado
  write.table(summary, summary_file_name, col.names = T, row.names = F, sep=",")
  
}



