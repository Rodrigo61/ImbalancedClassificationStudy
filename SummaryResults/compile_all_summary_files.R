library(stringr)
library(Jmisc)

SUMMARY_LIST_FILENAME = "summary_list"

system("./generate_summary_list.sh")


summary_list = as.vector(read.csv(SUMMARY_LIST_FILENAME, header = F))

df_final = NULL

for(summary_file_name in summary_list[,1]){
  
  summary_file_name = as.character(summary_file_name)
  summary = read.csv(summary_file_name, header = T)
  summary_dir_path = dirname(summary_file_name)
  dataset_imba_rate = str_extract(summary_file_name, "0.[0-9]{2,3}")
  dataset_name = basename(summary_dir_path)
  

print("dataset_imba_rate")
print(dataset_imba_rate)

  #Adicionando coluna 'dataset' e 'imba. rate'  
  summary = cbind(summary, dataset_name)
  names(summary)[ncol(summary)] = "dataset"
  summary = cbind(summary, dataset_imba_rate)
  names(summary)[ncol(summary)] = "imba. rate"

  df_final = rbind(df_final, summary)
}

#print("Summary final:")
#print(summary)

#Salvando dados do dataframe final
out_filename = "summary_compilation.csv"
write.table(df_final, out_filename, col.names = T, row.names = F, sep=",")
print(paste("Summary compilado e salvo em: ", out_filename, sep=""))
