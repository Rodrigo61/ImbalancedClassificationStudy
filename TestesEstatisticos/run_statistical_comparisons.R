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

rm(params)

for (performance in c(HOLDOUT, RESIDUAL, TUNING)){
  
  dir.create(paste("./outputs/", performance, "/", sep=""), showWarnings = FALSE)
  for (measure in c(ACC, AUC, MCC, GMEAN, F1)){

    dir.create(paste("./outputs/", performance, "/", measure, "/", sep=""), showWarnings = FALSE)  
    folder = paste("./outputs/", performance, "/", measure, "/", sep="")
    
    # 1-) (Algo+BD+desb)x(Tecnica)    
    rmarkdown::render(RMD_FILE, 
                      params = list(columns = TECNICAS, 
                                    measure = measure, 
                                    performance = performance),
                      output_file = paste(folder, "Algo+BD+desb(-VS-)Tecnica", performance, measure, ".pdf", sep="_"))    
    
    # 2-) (Algo+BD+desb_0.05)x(Tecnica)
    for(imba in c("0.05", "0.03", "0.01", "0.001")){
        rmarkdown::render(RMD_FILE, 
                          params = list(columns = TECNICAS, 
                                        measure = measure, 
                                        filter_keys = c("imba.rate"),
                                        filter_values = c(imba),
                                        performance = performance),
                          output_file = paste(folder, "Algo+BD", imba, "(-VS-)Tecnica", performance, measure, ".pdf", sep="_"))
    }
    
    # 3-) (BD+desb+tecnica_normal)x(Algo)
    rmarkdown::render(RMD_FILE, 
                      params = list(columns = c("learner"), 
                                    measure = measure, 
                                    filter_keys = c(TECNICAS),
                                    filter_values = c("FALSE", "FALSE", "FALSE"),
                                    performance = performance),
                      output_file = paste(folder, "BD+desb+tecnica_normal(-VS-)Algo", performance, measure, ".pdf", sep="_"))
    
    # 4-) (BD+desb)x(Algo+Tecnica)
    rmarkdown::render(RMD_FILE, 
                      params = list(columns = c(TECNICAS, "learner"), 
                                    measure = measure, 
                                    performance = performance),
                      output_file = paste(folder, "BD+desb(-VS-)Algo+Tecnica", performance, measure, ".pdf", sep="_"))
  }
  
}



