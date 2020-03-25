#Temp R1_13_1 analysis

library(ggplot2)
library(reshape2)
library(magrittr)
source("/home/ubuntu/PhD_proj/R_script/helper.R")
source("~/PhD_proj/pipeline/Script/Probabilistic_model.R")



filename = "~/PhD_proj/pipeline/Validation_11_19_2019/R1_13_1_merge.csv"



ncol = 50
score_f15_t4 = read.csv(file = filename,header = F,col.names = seq(1, ncol))
while(sum(is.numeric(score_f15_t4$X2)) > 0 || sum(is.na(score_f15_t4$X2))) {
  ncol = ncol + 50
  score_f15_t4 = read.csv(file = filename,header = F,col.names = seq(1, ncol))
}



n_junc = 9

for (juncID in 1:1){
nrow = dim(score_f15_t4)[1]
likelihood_score = score_f15_t4[c(seq(juncID,nrow, n_junc)),]
minimap_correctness = likelihood_score$X1
fast5_file = likelihood_score$X2

likelihood_score = cbind(minimap_correctness,fast5_file, likelihood_score[,seq(6,ncol,3)])
likelihood_score = likelihood_score[!is.na(likelihood_score$X6),]

#minimap acc
print(juncID)
print(mean(likelihood_score$minimap_correctness))
#nanoSplicer acc
print(mean(apply(likelihood_score[,c(-1,-2)],1, which.min) == 1))
print(length(likelihood_score$minimap_correctness))
}


select_row = (likelihood_score$minimap_correctness==0) * (apply(likelihood_score[,c(-1,-2)],1, which.min) > 1)
likelihood_score[select_row==1,]

