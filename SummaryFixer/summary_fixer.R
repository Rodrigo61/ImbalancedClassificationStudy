# Este módulo é responsável por preencher com linhas vazias as informacoes faltantes em cada summary_file
# dos data sets artificialmente desbalanceados. Ou seja, se um data set desbalanceado não teve alguma informacao
# computada que deveria ter sido, entre todas as combinacoes de cenários esperadas, este módulo deve preencher
# esse cenarios com NA's de modo que todos os summary_files contenham a mesma quantidade de linhas. Por fim essas
# linhas são ordenadas para que possamos comparar lado a lado quaisquer dois summary_files.
#




library(stringr)

#**************************************************************#
#*******************  FUNCOES   *******************************#
#**************************************************************#

update_stats = function(missing_stats, combination_id){
    #Funcao responsável por acumular os numeros e gerar as estatisticas de dados faltantes para cada cenário possivel.
    #Utilizamos para isso a variavel de programa 'missing_stats' para acumular os valores

    if(is.null(missing_stats[[combination_id]])){
      missing_stats[[combination_id]] <<- 1
    }else{
      missing_stats[[combination_id]] <<- missing_stats[[combination_id]] + 1
    }

}


fix_missing_combination = function(summary, missing_stats){
  # Verifica e resolve todas as técnicas utilizadas no estudo. IMPORTANTE: Supomos para esse métodos que não há mistura de técnicas, e.g,
  # SMOTE + CW

  measures = c('Accuracy', 'Area under the curve', "F1 measure", "G-mean", "Matthews correlation coefficient")
  learners = c('classif.ksvm', 'classif.randomForest', 'classif.xgboost')
  techniques = list()
  techniques$sampling = c('SMOTE', 'ADASYN')
  techniques$underbagging = c('TRUE')
  techniques$weight_space = c('TRUE')

  # Vamos fazer a busca para todas as combinacoes nesse summary
  for(learner in learners){
    for(measure in measures){

      # Buscando pelas tecnicas
      for (technique in names(techniques)) {
        technique_options = techniques[[technique]]
        for (option in technique_options){

          combination_count = length(which(summary[, 'learner'] == learner
                                           & summary[, 'measure'] == measure
                                           & summary[, technique] == option))



          # Nao existe medicao para essa combinacao, devemos gerar 3 linhas vazias entao
          if(combination_count == 0){
              print(paste("Combinacao faltante: learner = ", learner, " measure = ", measure, " technique = ", technique, " option = ", option, sep =""))

              empty_line = data.frame(learner, F, measure, F, F, NA, NA, NA, NA)
              names(empty_line) = names(summary)

              summary = rbind(summary, empty_line)
              summary = rbind(summary, empty_line)
              summary = rbind(summary, empty_line)

              # Inserimos todas as tecnicas como falsa e só atualizamos a tecnica da combinacao atual
              # isso só é possível pela suposicao de que as técnicas nao se misturam
              summary[(NROW(summary)-2):NROW(summary), technique] = option

              # Atualiza estatisticas
              combination_id = paste(learner, measure, technique, option, sep="_")
              update_stats(missing_stats, combination_id)
          }else if(combination_count != 3){
            print(paste("A combinacao a seguir tem um número inesperado de medicoes = ", combination_count ," (!= 0 & != 3) [ ", "leaner = ", learner, " measure = ", measure, " technique = ", technique, " option = ", option, " ]", sep =""))            
         }

        }
      }

      # Buscando por cenário normal (i.e. todas as técnicas como FALSE)
      combination_count = length(which(summary[,'learner'] == learner
                                & summary[, 'measure'] == measure
                                & apply(summary, 1, function(row){
                                  any(as.logical(row[c('weight_space', 'underbagging', 'sampling')]))
                                }) == F))

      # Nao existe medicao para essa combinacao, devemos gerar 3 linhas vazias entao
      if(combination_count == 0){
        print(paste("Combinacao faltante: learner = ", learner, " measure = ", measure, " technique = NULL (NORMAL)", sep =""))

        empty_line = data.frame(learner, F, measure, F, F, NA, NA, NA, NA)
        names(empty_line) = names(summary)

        summary = rbind(summary, empty_line)
        summary = rbind(summary, empty_line)
        summary = rbind(summary, empty_line)

        # Atualiza estatisticas
        combination_id = paste(learner, measure, technique, option, sep="_")
        update_stats(missing_stats, combination_id)
      }else if(combination_count != 3){
        print(paste("A combinacao a seguir tem um número inesperado de medicoes = ", combination_count ," (!= 0 & != 3) [ ", "leaner = ", learner, " measure = ", measure, " technique = NULL (NORMAL)]", sep =""))
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

# Inicializa estrutura de estatisticas gerais
missing_stats = list()

for(summary_file_name in summary_list[,1]){

  print(paste("Lendo o arquivo: ", summary_file_name, sep=''))
  summary_file_name = as.character(summary_file_name)
  summary = read.csv(summary_file_name, header = T)

  #TODO: Apagar essa conversao quanto arrumarmos o fato de que existem summary files com NA's ao inves de FALSE's na coluna 'sampling'
  if(!('FALSE' %in% levels(summary[,'sampling']))){
    levels(summary[,'sampling']) = c(levels(summary[,'sampling']), 'FALSE')
  }
  summary[which(is.na(summary[,'sampling'])),'sampling'] = 'FALSE'
  

  # Verifica todas as combinacoes que deveriam ter medicoes, mas nao tem e as completa com NA's
  summary = fix_missing_combination(summary, missing_stats)

  #Ordenando pelas colunas o summary
  summary = summary[do.call(order, lapply(1:NCOL(summary), function(i) summary[, i])), ]

  #Salvando summary atualizado
  write.table(summary, summary_file_name, col.names = T, row.names = F, sep=",")
  print(paste("Summary atualizado e salvo em: ", summary_file_name), sep="")

}

#Printando estatisticas
print("Estatisticas do script:")
print(missing_stats)
print("-----------------------------------------------")



