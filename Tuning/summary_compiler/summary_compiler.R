##
# Script responsável por compilar as informacoes obtidadas pelo tuning individual de
# cada uma das possibilidade de metrica X algoritmo X cenario em um soh CSV para o dataset
# em questao.
##

library(stringr)

# Lista de colunas da compilacao final, nem todos os arquivos individuais devem respeitar
# essa lista
COLUMNS_NAMES = c("learner", "weight_space", "measure", "sampling", 
                  "tuning_measure", "holdout_measure", 
                  "holdout_measure_residual", "iteration_count")

#**************************************************************#
#*******************  CONSTANTES   ****************************#
#**************************************************************#
SUMMARY_LIST_FILENAME = "summary_dir_list"
DEBUG = T

#**************************************************************#
#*******************  FUNCOES     *****************************#
#**************************************************************#
print_debug = function(str){
  if(!is.character(str)){
    str = paste(str, collapse = "")
  }
  if(DEBUG){
    print(paste("[DEBUG]", str, sep=""))
  }
}

#**************************************************************#
#*******************  MAIN   **********************************#
#**************************************************************#

#Lendo os parametros do script
args = commandArgs(trailingOnly=TRUE)

#Lendo lista de pastas de summary
summary_dir_list = read.csv(SUMMARY_LIST_FILENAME, header=F)
print_debug(summary_dir_list)

#Selecionando pasta de summary pela posicao na lista
summary_dir_id = as.numeric(args[1]) + 1
summary_dir_path = as.character(summary_dir_list[summary_dir_id,])
dataset_imba_rate = str_extract(summary_dir_path, "0.[0-9]{2,3}")

print_debug(summary_dir_path)

#Obtendo a lista de todos os arquivos da pasta summary selecionada
summary_file_list = list.files(summary_dir_path, full.names = T)

#Dataframe com a compilacao de todos os Dataframes dos arquivos da pasta de summary
df_final = NULL

#Iterando sobre todos os arquivos de summary e acumulando suas informacoes
for(file_path in summary_file_list){

  # Verificando se existe informacao repetida. Se o nome de um arquivo
  # for substring de outro (file_path for substring de other_file_path), 
  # isso significa que ele está desatualizadoe imcompleto, ficamos 
  # apenas com o mais atualizado.
  deleted_file = FALSE
  for(other_file_path in summary_file_list){
    if(other_file_path != file_path){
    if(grepl(file_path, other_file_path, fixed=T)){
      system(paste("rm ", file_path, sep=''))
      deleted_file = TRUE
      break
    }
    }
  }
  
  if(!deleted_file){
    #lendo o df presente no arquivo atual
    print_debug(paste("File path: ", file_path))
    df = read.csv(file_path, header = T)
  
    print_debug("Dataset corrent:")
    print(df)
    
    # Vamos adicionar ao arquivo de summary individual as colunas
    # que ele não era responsável por informar, mas que precisamos
    # formalizar no arquivo compilado final
    for(column in COLUMNS_NAMES){
      if(column %in% colnames(df) == FALSE){
        df = cbind(df, FALSE)
        colnames(df)[ncol(df)] = column
      }
    }
    
    # Rearranjando as colunas para a ordem correta
    df = df[, COLUMNS_NAMES]
    
    #acumulando no dataframe final
    df_final = rbind(df_final, df)
  }
}

#Salvando dados do dataframe final
out_filename = paste("ds_", dataset_imba_rate, "_summary.csv", sep ="")
out_path = paste(dirname(summary_dir_path), out_filename, sep="/")
write.table(df_final, out_path, col.names = T, row.names = F, sep=",")
print_debug(paste("Summary compilado e salvo em: ", out_path, sep=""))
