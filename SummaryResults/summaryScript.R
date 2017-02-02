library(ggplot2)
################################################################
###############   DEBUG FUNCTIONS   ############################
################################################################
DEBUG = T
print_debug = function(str){
  if(DEBUG){
    print(paste("[DEBUG]", str, sep=""))
  }
}

get_from_summary = function(data, learner, measure, is_weight_space, interation_count){
  index_list = which(data[,"weight_space"] == is_weight_space)
  index_list = which(data[index_list,"learner"] == learner)
  index_list = which(data[index_list,"time"] == interation_count)
  
  return(data[index_list, measure])
}
################################################################
###############  MAIN   ########################################
################################################################

#Cada gráfico tem uma lista de valores dedicada
svm_acc_list = NULL
svm_f1_list = NULL
svm_gmeans_list = NULL
svm_mcc_list = NULL
svm_wg_acc_list = NULL
svm_wg_f1_list = NULL
svm_wg_gmeans_list = NULL
svm_wg_mcc_list = NULL
rf_acc_list = NULL
rf_f1_list = NULL
rf_gmeans_list = NULL
rf_mcc_list = NULL
rf_wg_acc_list = NULL
rf_wg_f1_list = NULL
rf_wg_gmeans_list = NULL
rf_wg_mcc_list = NULL

#Lendo lista de 'summary' dos datasets
dataset_list = read.csv("summary_ds_list", header=F)

#Iterando sobre os 'summary' de cada dataset e acumulando os 
#valores na lista correta
for(i in 1:((dim(dataset_list)[1]) - 5)){
  
  summary_path = as.character(dataset_list[i,])  
  print_debug(paste("Lendo: ", summary_path, sep=""))  
  summary_df = read.csv(summary_path, header = T)
  
  #SVM
  svm_acc_list = c(svm_acc_list, get_from_summary(summary_df, "classif.ksvm", "acc_tuned", F, 1))
  svm_f1_list = c(svm_f1_list, get_from_summary(summary_df, "classif.ksvm", "f1_tuned", F, 1))
  svm_gmeans_list = c(svm_gmeans_list, get_from_summary(summary_df, "classif.ksvm", "gmeans_tuned", F, 1))
  svm_mcc_list = c(svm_mcc_list, get_from_summary(summary_df, "classif.ksvm", "mcc_tuned", F, 1))
  #SVM_WG
  svm_acc_list = c(svm_acc_list, get_from_summary(summary_df, "classif.ksvm", "acc_tuned", F, 1))
  svm_f1_list = c(svm_f1_list, get_from_summary(summary_df, "classif.ksvm", "f1_tuned", F, 1))
  svm_gmeans_list = c(svm_gmeans_list, get_from_summary(summary_df, "classif.ksvm", "gmeans_tuned", F, 1))
  svm_mcc_list = c(svm_mcc_list, get_from_summary(summary_df, "classif.ksvm", "mcc_tuned", F, 1))
  #RF
  svm_acc_list = c(svm_acc_list, get_from_summary(summary_df, "classif.ksvm", "acc_tuned", F, 1))
  svm_f1_list = c(svm_f1_list, get_from_summary(summary_df, "classif.ksvm", "f1_tuned", F, 1))
  svm_gmeans_list = c(svm_gmeans_list, get_from_summary(summary_df, "classif.ksvm", "gmeans_tuned", F, 1))
  svm_mcc_list = c(svm_mcc_list, get_from_summary(summary_df, "classif.ksvm", "mcc_tuned", F, 1))
  #RF_WG
  svm_acc_list = c(svm_acc_list, get_from_summary(summary_df, "classif.ksvm", "acc_tuned", F, 1))
  svm_f1_list = c(svm_f1_list, get_from_summary(summary_df, "classif.ksvm", "f1_tuned", F, 1))
  svm_gmeans_list = c(svm_gmeans_list, get_from_summary(summary_df, "classif.ksvm", "gmeans_tuned", F, 1))
  svm_mcc_list = c(svm_mcc_list, get_from_summary(summary_df, "classif.ksvm", "mcc_tuned", F, 1))
}


#Gerando gráfico de ponto e linha com a lista de dados obtida
print_debug("Gerando imagens na pasta...")
jpeg('svm_data_acc.jpg')
plot(svm_acc_list,type="l",col="red")
lines(svm_wg_acc_list, col="green")
garbage = dev.off()

jpeg('svm_data_f1.jpg')
plot(svm_acc_list,type="l",col="red")
lines(svm_wg_acc_list, col="green")
garbage =dev.off()

jpeg('svm_data_gmeans.jpg')
plot(svm_acc_list,type="l",col="red")
lines(svm_wg_acc_list, col="green")
garbage = dev.off()

jpeg('svm_data_mcc.jpg')
plot(svm_mcc_list,type="l",col="red")
lines(svm_wg_mcc_list, col="green")
garbage = dev.off()

print_debug("Imagens geradas!")