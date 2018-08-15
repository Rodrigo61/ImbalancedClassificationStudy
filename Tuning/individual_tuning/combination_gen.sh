#!/bin/bash

# Este script é responsável pela criacao de forma automática de jobs para serem executados pelo Condor.
# É um script de execucao manual (i.e. deve ser chamado pelo usuario antes de executar os jobs).
# Ele cria em uma pasta de submissao todos os pares de arquivo .sh e .sub de todas as combinacoes possíveis de execucao 
# do script tuning.R
# São responsabilidades deste script:
#	* Listar todas as métricas a serem utilizadas
#	* Listar todos os Learners a serem utilizados
#	* Listar todos os algoritmos de sampling a serem utilizados
#	* Adicionar novas lógicas e listagem para novas categorias de combinacao


## vetor de metricas
declare -a measures=("acc" "f1" "gmeans" "mcc" "auc" "bac")

## vetor de algoritmos
declare -a learners=("svm" "rf" "xgboost" "rpart" "knn")

## vetor de algoritmos oversampling
declare -a oversamplings=("smote" "adasyn")


## criando caso nao exista o diretório submission_files
submission_files_dir="submission_files"
mkdir -p $submission_files_dir
echo "pasta 'submission_files' criado no diretorio $HOME_PATH"


## recriando script 'run_all.sh'
run_all_path="run_all.sh"
echo "#!/bin/bash" > $run_all_path
echo "cd $submission_files_dir" >> $run_all_path
chmod 755 $run_all_path


# Agora vamos trabalhar dentro do diretorio de arquivos de submissao
cd $submission_files_dir


## criando os pares de arquivos (.sh) e (.sub) de todas as combinacoes
for measure in "${measures[@]}"
do
	for learner in "${learners[@]}"
	do

###############
#Begin Normal (sem técnica de tratamento)
###############	

		#gerando arquivo com aprendizado normal (.SH)
		normal_file="${measure}_${learner}.sh"
		content_normal='Rscript --vanilla ../tuning.R --dataset_id=$@ --measure='$measure' --model='$learner''
		echo "#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
$content_normal" > $normal_file
		chmod 755 $normal_file

		#gerando arquivo com aprendizado normal (.sub)
		normal_file_sub="${measure}_${learner}.sub"
		echo 'N=228
universe                = vanilla
executable            = '$normal_file'
arguments               = $(Process)
Requirements = ((Machine == "cerebro.recod") || (Machine == "deepthought.recod")  || (Machine == "eddie.recod") || (Machine == "magi.recod") || (Machine == "theengine.recod") || (Machine == "twin01-a.recod") || (Machine == "twin01-b.recod") || (Machine == "twin01-c.recod") || (Machine == "twin01-d.recod") || (Machine == "twin02-a.recod") || (Machine == "twin02-c.recod") || (Machine == "viki.recod") || (Machine == "x04.recod") )
output                = condor.out.$(CLUSTER).$(Process)
log                     = condor.log.$(CLUSTER).($Process)
error                   = condor.err.$(CLUSTER).$(Process)

queue $(N) ' > $normal_file_sub
		echo "sleep 20 | condor_submit $normal_file_sub" >> ../$run_all_path # append no run_all.sh


###############
#Begin Cost-sensitive learning
###############	

		#gerando arquivo com aprendizado Cost-sensitive learning
		ws_file="${measure}_${learner}_weight.sh"
		content_ws='Rscript --vanilla ../tuning.R --dataset_id=$@ --measure='$measure' --model='$learner' --weight_space'
		echo "#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
$content_ws" > $ws_file
		chmod 755 $ws_file

		#gerando arquivo com aprendizado Cost-sensitive learning (.sub)
		ws_file_sub="${measure}_${learner}_weight.sub"
		echo 'N=228
universe                = vanilla
executable            = '$ws_file'
arguments               = $(Process)
Requirements = ((Machine == "cerebro.recod") || (Machine == "deepthought.recod")  || (Machine == "eddie.recod") || (Machine == "magi.recod") || (Machine == "theengine.recod") || (Machine == "twin01-a.recod") || (Machine == "twin01-b.recod") || (Machine == "twin01-c.recod") || (Machine == "twin01-d.recod") || (Machine == "twin02-a.recod") || (Machine == "twin02-c.recod") || (Machine == "viki.recod") || (Machine == "x04.recod") )
output                = condor.out.$(CLUSTER).$(Process)
log                     = condor.log.$(CLUSTER).($Process)
error                   = condor.err.$(CLUSTER).$(Process)

queue $(N) ' > $ws_file_sub
		echo "sleep 20 | condor_submit $ws_file_sub" >> ../$run_all_path # append no run_all.sh

###############
#Begin OVERSAMPLING
###############	
		#gerando arquivo  com algoritmos de oversampling
		for oversampling in "${oversamplings[@]}"
		do
			#Gerando o (.sh)
			oversampling_file="${measure}_${learner}_${oversampling}.sh"
			content_oversampling='Rscript --vanilla ../tuning.R --dataset_id=$@ --measure='$measure' --model='$learner' --oversampling='$oversampling''
			echo "#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
$content_oversampling" > $oversampling_file
			chmod 755 $oversampling_file

			#Gerando o (.sub)
			oversampling_file_sub="${measure}_${learner}_${oversampling}.sub"
			echo 'N=228
universe                = vanilla
executable            = '$oversampling_file'
arguments               = $(Process)
Requirements = ((Machine == "cerebro.recod") || (Machine == "deepthought.recod")  || (Machine == "eddie.recod") || (Machine == "magi.recod") || (Machine == "theengine.recod") || (Machine == "twin01-a.recod") || (Machine == "twin01-b.recod") || (Machine == "twin01-c.recod") || (Machine == "twin01-d.recod") || (Machine == "twin02-a.recod") || (Machine == "twin02-c.recod") || (Machine == "viki.recod") || (Machine == "x04.recod") )
output                = condor.out.$(CLUSTER).$(Process)
log                     = condor.log.$(CLUSTER).($Process)
error                   = condor.err.$(CLUSTER).$(Process)

queue $(N) ' > $oversampling_file_sub
			echo "sleep 20 | condor_submit $oversampling_file_sub" >> ../$run_all_path # append no run_all.sh
		done

###############
#Begin underbagging
###############		
		
		#Gerando o (.sh)
		underbagging_file="${measure}_${learner}_underbagging.sh"
		content_underbagging='Rscript --vanilla ../tuning.R --dataset_id=$@ --measure='$measure' --model='$learner' --underbagging'
		echo "#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
$content_underbagging" > $underbagging_file
		chmod 755 $underbagging_file

		#Gerando o (.sub)
		underbagging_file_sub="${measure}_${learner}_underbagging.sub"
		echo 'N=228
universe                = vanilla
executable            = '$underbagging_file'
arguments               = $(Process)
Requirements = ((Machine == "cerebro.recod") || (Machine == "deepthought.recod")  || (Machine == "eddie.recod") || (Machine == "magi.recod") || (Machine == "theengine.recod") || (Machine == "twin01-a.recod") || (Machine == "twin01-b.recod") || (Machine == "twin01-c.recod") || (Machine == "twin01-d.recod") || (Machine == "twin02-a.recod") || (Machine == "twin02-c.recod") || (Machine == "viki.recod") || (Machine == "x04.recod") )
output                = condor.out.$(CLUSTER).$(Process)
log                     = condor.log.$(CLUSTER).($Process)
error                   = condor.err.$(CLUSTER).$(Process)

queue $(N) ' > $underbagging_file_sub
		echo "sleep 20 | condor_submit $underbagging_file_sub" >> ../$run_all_path # append no run_all.sh
		
###############
#END FOR Learners
###############
    done

###############
#Begin RUSBoost
###############	
    	#gerando arquivo do RUSBoost
	#Gerando o (.sh)
	ensemble_file="${measure}_rusboost.sh"
	content_ensemble='Rscript --vanilla ../tuning.R --dataset_id=$@ --measure='$measure' --model=rusboost'
    	echo "#!/bin/bash
export PATH=/home/rodrigoaf/R-3.3.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
$content_ensemble" > $ensemble_file
	
    	chmod 755 $ensemble_file

	#Gerando o (.sub)
	ensemble_file_sub="${measure}_rusboost.sub"
	echo 'N=228
universe                = vanilla
executable            = '$ensemble_file'
arguments               = $(Process)
Requirements = ((Machine == "cerebro.recod") || (Machine == "deepthought.recod")  || (Machine == "eddie.recod") || (Machine == "magi.recod") || (Machine == "theengine.recod") || (Machine == "twin01-a.recod") || (Machine == "twin01-b.recod") || (Machine == "twin01-c.recod") || (Machine == "twin01-d.recod") || (Machine == "twin02-a.recod") || (Machine == "twin02-c.recod") || (Machine == "viki.recod") || (Machine == "x04.recod") )
output                = condor.out.$(CLUSTER).$(Process)
log                     = condor.log.$(CLUSTER).($Process)
error                   = condor.err.$(CLUSTER).$(Process)

queue $(N) ' > $ensemble_file_sub
	echo "sleep 20 | condor_submit $ensemble_file_sub" >> ../$run_all_path # append no run_all.sh

###############
#END FOR Measures
###############
done


echo "Arquivos de submissão e script foram criados"
