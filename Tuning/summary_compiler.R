##
# Script responsável por compilar as informacoes obtidadas pelo tuning individual de
# cada uma das possibilidade de metrica X algoritmo X cenario em um soh CSV para o dataset
# em questao.
##


summary_dir_path

#Obtendo a lista de todos os arquivos da pasta summary
summary_file_list = list.files(summary_file_list, full.names = T)


#Iterando sobre todos os arquivos de summary e acumulando suas informacoes
for(file_path in (1:length(summary_file_list))){
  
  #lendo o df presente no arquivo atual
  df = read.csv(summary_path, header = T)
  
}