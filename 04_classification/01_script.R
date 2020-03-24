library(xgboost)
library(caret) #confusion matrix
library(caTools) #sample split
library(Hmisc)
library(tidyverse)
library(pROC)

source("helper_functions/xgb_best_iter.R")
source("helper_functions/xgb_fts_importance.R")
#
t_path_mut_score <- read.csv("../01_identification_of_mutation_subtypes/ensemble_clustering/t_path_mut_score",
                             row.names = 1)
#
cor_res2 <- rcorr(as.matrix(t_path_mut_score))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

cor_res3 <- flattenCorrMatrix(cor_res2$r, cor_res2$P)

correlated_fts <- cor_res3[order(cor_res3$cor,decreasing=T),"column"][cor_res3[order(cor_res3$cor,decreasing=T),"cor"] > 0.7]
correlated_fts <- correlated_fts[!duplicated(correlated_fts)]

####
t_path_mut_score_mod <- t_path_mut_score[,!colnames(t_path_mut_score)  %in% correlated_fts]
t_path_mut_score_mod$cluster_k5 <- donor_cp$cluster_k5
t_path_mut_score_mod_k5 <- t_path_mut_score_mod[t_path_mut_score_mod$cluster_k5 != 4,]

t_path_mut_score_mod_k5[t_path_mut_score_mod_k5$cluster_k5 == 1, "cluster_k5"] <- 0
t_path_mut_score_mod_k5[t_path_mut_score_mod_k5$cluster_k5 == 2, "cluster_k5"] <- 1
t_path_mut_score_mod_k5[t_path_mut_score_mod_k5$cluster_k5 == 3, "cluster_k5"] <- 2
t_path_mut_score_mod_k5[t_path_mut_score_mod_k5$cluster_k5 == 5, "cluster_k5"] <- 3

## split data
set.seed(42)
train_07_k5 <- sample.split(t_path_mut_score_mod_k5$cluster_k5, SplitRatio = 0.8) #i.e. 80 % in training data
train_df_07_k5 <-  t_path_mut_score_mod_k5[train_07_k5, ]
test_df_07_k5 <- t_path_mut_score_mod_k5[!train_07_k5, ]

## best iteration
best_it_e3_md4 <- xgb_best_iter(train_df_07_k5,4, 318, 0.3, 4, "mlogloss")
best_it_e3_md6 <- xgb_best_iter(train_df_07_k5,4, 318, 0.3, 6, "mlogloss")
best_it_e3_md9 <- xgb_best_iter(train_df_07_k5,4, 318, 0.3, 9, "mlogloss")

best_it_e1_md4 <- xgb_best_iter(train_df_07_k5,4, 318, 0.1, 4, "mlogloss")
best_it_e1_md6 <- xgb_best_iter(train_df_07_k5,4, 318, 0.1, 6, "mlogloss")
best_it_e1_md9 <- xgb_best_iter(train_df_07_k5,4, 318, 0.1, 9, "mlogloss")

best_it_e05_md4 <- xgb_best_iter(train_df_07_k5,4, 318, 0.05, 4, "mlogloss")
best_it_e05_md6 <- xgb_best_iter(train_df_07_k5,4, 318, 0.05, 6, "mlogloss")
best_it_e05_md9 <- xgb_best_iter(train_df_07_k5,4, 318, 0.05, 9, "mlogloss")

best_it_e01_md4 <- xgb_best_iter(train_df_07_k5,4, 318, 0.01, 4, "mlogloss")
best_it_e01_md6 <- xgb_best_iter(train_df_07_k5,4, 318, 0.01, 6, "mlogloss")
best_it_e01_md9 <- xgb_best_iter(train_df_07_k5,4, 318, 0.01, 9, "mlogloss")

## feature importance
fst_e3_md4 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e3_md4, 0.3, 4, "mlogloss")
fst_e3_md6 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e3_md6, 0.3, 6, "mlogloss")
fst_e3_md9 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e3_md9, 0.3, 9, "mlogloss")

fst_e1_md4 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e1_md4, 0.1, 4, "mlogloss")
fst_e1_md6 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e1_md6, 0.1, 6, "mlogloss")
fst_e1_md9 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e1_md9, 0.1, 9, "mlogloss")

fst_e05_md4 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e05_md4, 0.05, 4, "mlogloss")
fst_e05_md6 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e05_md6, 0.05, 6, "mlogloss")
fst_e05_md9 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e05_md9, 0.05, 9, "mlogloss")

fst_e01_md4 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e01_md4, 0.01, 4, "mlogloss")
fst_e01_md6 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e01_md6, 0.01, 6, "mlogloss")
fst_e01_md9 <- xgb_fts_importance(train_df_07_k5,4, 318,best_it_e01_md9, 0.01, 9, "mlogloss")

## evaluation
fst_e3_md4[[2]]$evaluation_log[fst_e3_md4[[2]]$best_iteration,4]
fst_e3_md6[[2]]$evaluation_log[fst_e3_md6[[2]]$best_iteration,4]
fst_e3_md9[[2]]$evaluation_log[fst_e3_md9[[2]]$best_iteration,4]

fst_e1_md4[[2]]$evaluation_log[fst_e1_md4[[2]]$best_iteration,4]
fst_e1_md6[[2]]$evaluation_log[fst_e1_md6[[2]]$best_iteration,4]
fst_e1_md9[[2]]$evaluation_log[fst_e1_md9[[2]]$best_iteration,4]

fst_e05_md4[[2]]$evaluation_log[fst_e05_md4[[2]]$best_iteration,4] 
fst_e05_md6[[2]]$evaluation_log[fst_e05_md6[[2]]$best_iteration,4]
fst_e05_md9[[2]]$evaluation_log[fst_e05_md9[[2]]$best_iteration,4]

fst_e01_md4[[2]]$evaluation_log[fst_e01_md4[[2]]$best_iteration,4] ###best
fst_e01_md6[[2]]$evaluation_log[fst_e01_md6[[2]]$best_iteration,4]
fst_e01_md9[[2]]$evaluation_log[fst_e01_md9[[2]]$best_iteration,4]
