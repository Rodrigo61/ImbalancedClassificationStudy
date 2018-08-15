# Script responsável por utilizar os scripts RMDs para gerar todos os cenários de comparacao previstos
# veja o README deste módulo para melhores explicacoes.


RMD_FILE = "/home/rodrigo/Dropbox/UNICAMP/IC/estudo_cost_learning/TestesEstatisticos/statistical_comparisons.Rmd"

ACC = "Accuracy"
MCC = "Matthews correlation coefficient"
AUC = "Area under the curve"
GMEAN = "G-mean"
F1 = "F1 measure"

TECNICAS = c("sampling", "weight_space", "underbagging") 

HOLDOUT = "holdout_measure"
RESIDUAL = "holdout_measure_residual"
TUNING = "tuning_measure"


for (performance in c(HOLDOUT, RESIDUAL, TUNING)){
  
  dir.create(paste("/home/rodrigo/Dropbox/UNICAMP/IC/estudo_cost_learning/TestesEstatisticos/outputs/", performance, "/", sep=""), showWarnings = FALSE)
  #for (measure in c(ACC, AUC, MCC, GMEAN, F1)){

    # TODO: O FOR foi removido para gerar apenas AUC 
    measure = AUC
    
    
    dir_name = paste("/home/rodrigo/Dropbox/UNICAMP/IC/estudo_cost_learning/TestesEstatisticos/outputs/", performance, "/", measure, "/", sep="")
    if(!dir.exists(dir_name)){
      dir.create(dir_name, showWarnings = FALSE)    
    }
    
    folder = paste("/home/rodrigo/Dropbox/UNICAMP/IC/estudo_cost_learning/TestesEstatisticos/outputs/", performance, "/", measure, "/", sep="")
    
    # 1-) (Algo+BD+desb)x(Tecnica)    
    rmarkdown::render(RMD_FILE, 
                      params = list(columns = TECNICAS, 
                                    measure = measure, 
                                    performance = performance),
                      output_file = paste(folder, "Algo+BD+desb-VS-Tecnica", performance, measure, ".pdf", sep="_"))
    
    # 2-) (Algo+BD+desb_0.05)x(Tecnica)
    for(imba in c("0.05", "0.03", "0.01", "0.001")){
        rmarkdown::render(RMD_FILE, 
                          params = list(columns = TECNICAS, 
                                        measure = measure, 
                                        filter_keys = c("imba.rate"),
                                        filter_values = c(imba),
                                        performance = performance),
                          output_file = paste(folder, "Algo+BD", imba, "-VS-Tecnica", performance, measure, ".pdf", sep="_"))
    }
    
    # 3-) (BD+desb+tecnica_normal)x(Algo)
    rmarkdown::render(RMD_FILE, 
                      params = list(columns = c("learner"), 
                                    measure = measure, 
                                    filter_keys = c(TECNICAS),
                                    filter_values = c("FALSE", "FALSE", "FALSE"),
                                    performance = performance),
                      output_file = paste(folder, "BD+desb+tecnica_normal-VS-Algo", performance, measure, ".pdf", sep="_"))
    
    # 4-) (BD+desb_fixo+tecnica_normal)x(Algo)
    for(imba in c("0.05", "0.03", "0.01", "0.001")){
       rmarkdown::render(RMD_FILE, 
                        params = list(columns = c("learner"), 
                                      measure = measure, 
                                      filter_keys = c(TECNICAS, "imba.rate"),
                                      filter_values = c("FALSE", "FALSE", "FALSE", imba),
                                      performance = performance),
                        output_file = paste(folder, "BD", imba, "+tecnica_normal-VS-Algo", performance, measure, ".pdf", sep="_"))
    }
    
    # 5-) (BD+desb)x(Algo+Tecnica)
    rmarkdown::render(RMD_FILE, 
                      params = list(columns = c(TECNICAS, "learner"), 
                                    measure = measure, 
                                    performance = performance),
                      output_file = paste(folder, "BD+desb-VS-Algo+Tecnica", performance, measure, ".pdf", sep="_"))
    
    # 6-) (BD+desb_fixo)x(Algo+Tecnica)
    for(imba in c("0.05", "0.03", "0.01", "0.001")){
      rmarkdown::render(RMD_FILE, 
                        params = list(columns = c(TECNICAS, "learner"), 
                                      measure = measure, 
                                      filter_keys = c("imba.rate"),
                                      filter_values = c(imba),
                                      performance = performance),
                        output_file = paste(folder, "BD", imba, "-VS-Algo+Tecnica", performance, measure, ".pdf", sep="_"))
    }
  #}
  
}



