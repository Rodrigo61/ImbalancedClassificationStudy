#####################################
############ CONSTANTES #############
#####################################

#DATASET_LIST_NAME = "original_dataset_list"
DATASET_LIST_NAME = "original_dataset_list_RECOD"
#####################################
############ FUNCOES ################
#####################################

#Funcao de criacao dos datasets desbalanceados
#Essa funcao recebe como parametros os preditores(x_data) e a classe(y_data) de um dataset. Recebe como parametro a porcentagem exigida de classe minoritária para um novo dataset. 
#É feito apenas undersampling, nenhum dado é replicado. O undersampling sempre favorece a retirada da classe 
#majoritária, assim preservando o maximo possível a classe minoritária. 
#As observacoes que serão descartadas para possibilitar o desbalanceamento artificial também sao dadas como retorno.
imba_sample = function(x_data, y_data, minority_percent, warning=T){
  
  #Variavel de retorno
  ret = NULL
  
  #Variavel de armazenamento de obs. que serão descartadas para o desbalanceamento artificial.
  residual_indexes = NULL
  
  if(length(which(y_data == 1)) < 10){
    if(warning == T){
      warning("A classe minoritária nao tem observacoes suficientes. [Menos de 10 observacoes]")
    }
    return()
  }
  
  minority_indexes = which(y_data == 1)
  majority_indexes = which(y_data == 0)
  
  #Devemos ter no mínimo 10 observacoes da classe minoritária(regra do estudo)
  while(length(minority_indexes) >= 10){
    
    #Calculamos o necessário de obs. da classe majoritária para que a classe minoritária represente
    # o valor de 'minority_percent' no conjunto total
    needed_majority_abs_count = ceiling(length(minority_indexes)*(1-minority_percent)/minority_percent)
    
    if(length(majority_indexes) >= needed_majority_abs_count){
      
      #Indices do novo dataset desbalanceado
      new_indexes = c(minority_indexes, majority_indexes[1:needed_majority_abs_count])
      
      #Guardando todos as obs. da classe majoritaria que seriam eliminadas na lista de residuos
      residual_indexes = c(residual_indexes, majority_indexes[needed_majority_abs_count+1 : length(majority_indexes)])
      
      #Criando dataset desbalanceado de retorno da funcao
      imba_dataset = cbind(x_data[new_indexes,], y_data[new_indexes])
      names(imba_dataset)[ncol(imba_dataset)] = 'y_data'
      ret$imba_dataset = imba_dataset
      
      #Criando dataset de residuos de retorno da funcao
      residual_dataset = cbind(x_data[residual_indexes,], y_data[residual_indexes])
      names(residual_dataset)[ncol(residual_dataset)] = 'y_data'
      ret$residual_dataset = residual_dataset
      
      return(ret)
    }else{
      #Caso não tenhamos o numero necessário de obs. da classe majoritária, nós retiramos uma obs. da minoritária
      # e tentamos novamente.
      
      #removendo um indice da minoritaria, mas antes armazenando na lista de residuos
      residual_indexes = c(residual_indexes, minority_indexes[1])
      minority_indexes = minority_indexes[-1]
    }
  }
  
  if(warning == T){
    warning("A classe minoritária nao tem observacoes suficientes.")
  }
  return()
  
}

# Funcao que calcula a melhor escolha de classe minoritaria em um dataset binario/nao binario.
# Utilizando Brute force para calcular o maior numero possivel de datasets. i.e. Procurando
# qual é a melhor escolha para considerarmos como classe minoritária entre toda as classes
# presentes no dataset. Isso é feito escolhendo a classe com a qual conseguimos gerar o maior
# numero de desbalanceamentos artificias
find_minority_class = function(y_data){

  max_ds_gen = 0
  max_ds_gen_class = 0
  max_ds_gen_class_size = 0
  for(minority_class in unique(y_data)){
    
    #Binarizando o dataset (1 = minoritária, 0 = majoritária)
    #obs: Todas as outras classes são compiladas em uma só majoritária
    y_data_bin = replace(y_data, y_data == minority_class, -1)
    y_data_bin = replace(y_data_bin, y_data_bin != -1, -2)
    y_data_bin = replace(y_data_bin, y_data_bin == -1, 1)
    y_data_bin = replace(y_data_bin, y_data_bin == -2, 0)
    
    
    #Gerando todos os possiveis datasets. Nesse momento nao nos preocupamos com os warnings
    ds_0.05 = imba_sample(x_data, y_data_bin, 0.05, warning = F)
    ds_0.03 = imba_sample(x_data, y_data_bin, 0.03, warning = F)
    ds_0.01 = imba_sample(x_data, y_data_bin, 0.01, warning = F)
    ds_0.001 = imba_sample(x_data, y_data_bin, 0.001, warning = F)
    
    
    #Calculando quantos Datasets foram possiveis de serem gerados
    gen_ds_count = 0
    if(length(ds_0.05) != 0){
      gen_ds_count = gen_ds_count + 1
    }
    if(length(ds_0.03) != 0){
      gen_ds_count = gen_ds_count + 1
    }
    if(length(ds_0.01) != 0){
      gen_ds_count = gen_ds_count + 1
    }
    if(length(ds_0.001) != 0){
      gen_ds_count = gen_ds_count + 1
    }
    
    # Armazenando se essa escolha de classe minoritária foi a melhor até entao
    if(gen_ds_count >= max_ds_gen){
      max_ds_gen = gen_ds_count
      max_ds_gen_class = minority_class
      max_ds_gen_class_size = length(which(y_data_bin == minority_class))
    }
  }
  
  return(max_ds_gen_class)
}

#Binarizando o dataset (1 = minoritária, 0 = majoritária)
#obs: Todas as outras classes são compiladas em uma só majoritária
dataset_to_binary_form = function(y_data, minority_class){
  
  y_data_bin = replace(y_data, y_data == minority_class, -1)
  y_data_bin = replace(y_data_bin, y_data_bin != -1, -2)
  y_data_bin = replace(y_data_bin, y_data_bin == -1, 1)
  y_data_bin = replace(y_data_bin, y_data_bin == -2, 0)
  
  return(y_data_bin)
}

#####################################
############ MAIN ###################
#####################################

#Lendo lista de datasets
dataset_list = read.csv(DATASET_LIST_NAME, header=F)


#Selecionando dataset pela posicao na lista
args = commandArgs(trailingOnly=TRUE)

dataset_id = as.numeric(args[1]) + 1
print(paste("Dataset ID", dataset_id, sep = " "))

dataset_path = as.character(dataset_list[dataset_id,])
print(paste("Dataset path", dataset_path, sep = " "))

dataset_dir = dirname(dataset_path)

#Carregando dataset
dataset = read.csv(dataset_path, header = T)

#Separando em preditores e classe. Assumimos que a ultima coluna é a coluna das classes
x_data = dataset[,-ncol(dataset)]
y_data = dataset[,ncol(dataset)]

#Convertendo os labels(classes) para numerico
y_data = as.numeric(y_data)

minority_class = find_minority_class(y_data)

#Binarizando o dataset (1 = minoritária, 0 = majoritária)
#obs: Todas as outras classes são compiladas em uma só majoritária
y_data_bin = dataset_to_binary_form(y_data, minority_class)


#Gerando Datasets e seus residuos
imba_0.05 = imba_sample(x_data, y_data_bin, 0.05, warning = T)
imba_0.03 = imba_sample(x_data, y_data_bin, 0.03, warning = T)
imba_0.01 = imba_sample(x_data, y_data_bin, 0.01, warning = T)
imba_0.001 = imba_sample(x_data, y_data_bin, 0.001, warning = T)

#Apenas os Datasets desbalanceados
ds_0.05 = imba_0.05$imba_dataset
ds_0.03 = imba_0.03$imba_dataset
ds_0.01 = imba_0.01$imba_dataset
ds_0.001 = imba_0.01$imba_dataset

#Apenas os residuos
residual_0.05 = imba_0.05$residual_dataset
residual_0.03 = imba_0.03$residual_dataset
residual_0.01 = imba_0.01$residual_dataset
residual_0.001 = imba_0.01$residual_dataset

#Salvando datasets
if(length(ds_0.05) != 0){
  print(paste("Gerado dataset da classe de 0.05 de desbalanceamento com exatamente", 
              length(which(ds_0.05 == 1))/dim(ds_0.05)[1], 
              "de desbalanceamento", 
              sep = " "))
  
  #Escrevendo dataset
  file = paste(dataset_dir, "/ds_0.05.csv", sep="")
  write.table(ds_0.05, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Dataset:", file, sep=" "))
  #Escrevendo residuo
  file = paste(dataset_dir, "/residual_0.05.csv", sep="")
  write.table(residual_0.05, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Residuo:", file, sep=" "))
  
}else{
  print("Nao foi possivel criar para esse dataset um desbalanceamento de 0.05")
  write.table(ds_0.05, file=paste(dataset_dir, "/ds_0.05FALHOU.csv", sep=""), col.names = T, row.names = F, sep=",")
}

if(length(ds_0.03) != 0){
  print(paste("Gerado dataset da classe de 0.03 de desbalanceamento com exatamente", 
              length(which(ds_0.03 == 1))/dim(ds_0.03)[1], 
              "de desbalanceamento", 
              sep = " "))

  #Escrevendo dataset
  file = paste(dataset_dir, "/ds_0.03.csv", sep="")
  write.table(ds_0.03, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Dataset:", file, sep=" "))
  #Escrevendo residuo
  file = paste(dataset_dir, "/residual_0.03.csv", sep="")
  write.table(residual_0.03, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Residuo:", file, sep=" "))
}else{
  print("Nao foi possivel criar para esse dataset um desbalanceamento de 0.03")
  write.table(ds_0.03, file=paste(dataset_dir, "/ds_0.03FALHOU.csv", sep=""), col.names = T, row.names = F, sep=",")
}

if(length(ds_0.01) != 0){
  print(paste("Gerado dataset da classe de 0.01 de desbalanceamento com exatamente", 
              length(which(ds_0.01 == 1))/dim(ds_0.01)[1], 
              "de desbalanceamento", 
              sep = " "))
  
  #Escrevendo dataset
  file = paste(dataset_dir, "/ds_0.01.csv", sep="")
  write.table(ds_0.01, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Dataset:", file, sep=" "))
  #Escrevendo residuo
  file = paste(dataset_dir, "/residual_0.01.csv", sep="")
  write.table(residual_0.01, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Residuo:", file, sep=" "))
}else{
  print("Nao foi possivel criar para esse dataset um desbalanceamento de 0.01")
  write.table(ds_0.01, file=paste(dataset_dir, "/ds_0.01FALHOU.csv", sep=""), col.names = T, row.names = F, sep=",")
}

if(length(ds_0.001) != 0){
  print(paste("Gerado dataset da classe de 0.001 de desbalanceamento com exatamente", 
              length(which(ds_0.001 == 1))/dim(ds_0.001)[1], 
              "de desbalanceamento", 
              sep = " "))
  
  #Escrevendo dataset
  file = paste(dataset_dir, "/ds_0.001.csv", sep="")
  write.table(ds_0.001, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Dataset:", file, sep=" "))
  #Escrevendo residuo
  file = paste(dataset_dir, "/residual_0.001.csv", sep="")
  write.table(residual_0.001, file = file, col.names = T, row.names = F, sep=",")
  print(paste("Residuo:", file, sep=" "))
}else{
  print("Nao foi possivel criar para esse dataset um desbalanceamento de 0.001")
  write.table(ds_0.001, file=paste(dataset_dir, "/ds_0.001FALHOU.csv", sep=""), col.names = T, row.names = F, sep=",")
}
