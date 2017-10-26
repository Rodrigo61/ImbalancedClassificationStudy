
csv_filepath = "~/Dropbox/UNICAMP/IC/estudo_cost_learning/SummaryResults/summary_compilation.csv"
csv_fixed_filepath = "~/Dropbox/UNICAMP/IC/estudo_cost_learning/SummaryResults/summary_compilation_fixed.csv"
ds = read.csv(csv_filepath, header = T)
ds[which(is.na(ds[,'sampling'])),'sampling'] = F
write.table(ds, csv_fixed_filepath, col.names = T, row.names = F, sep=",")

print("Todos os NA's da coluna 'sampling' foram substituidos por FALSE")

# 
# csv_fixed_filepath = "~/Dropbox/UNICAMP/IC/estudo_cost_learning/SummaryResults/summary_compilation_fixed.csv"
# write.table(ds_w8_rusboost, csv_fixed_filepath, col.names = T, row.names = F, sep=",")

