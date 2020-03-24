library(xgboost)
library(caret) 
library(caTools) 

t_path_mut_score$cluster_k2 <- donor_cp$cluster_k2

t_path_mut_score[t_path_mut_score$cluster_k2 == 1,"cluster_k2"] <- 0
t_path_mut_score[t_path_mut_score$cluster_k2 == 2,"cluster_k2"] <- 1

##split data
set.seed(42)
train_07 <- sample.split(t_path_mut_score$cluster_k2, SplitRatio = 0.8) #i.e. 80 % in training data
train_df_07 <-  t_path_mut_score[train_07, ]
test_df_07 <- t_path_mut_score[!train_07, ]

source("../04_classification/helper_functions/xgb_best_iter_bin.R")
source("../04_classification/helper_functions/xgb_fts_importance_bin.R")

## best iteration
best_it_e3_md4 <- xgb_best_iter_bin(train_df_07, 198, 0.3, 4, "logloss")
best_it_e3_md6 <- xgb_best_iter_bin(train_df_07, 198, 0.3, 6, "logloss")
best_it_e3_md9 <- xgb_best_iter_bin(train_df_07, 198, 0.3, 9, "logloss")

best_it_e1_md4 <- xgb_best_iter_bin(train_df_07, 198, 0.1, 4, "logloss")
best_it_e1_md6 <- xgb_best_iter_bin(train_df_07, 198, 0.1, 6, "logloss")
best_it_e1_md9 <- xgb_best_iter_bin(train_df_07, 198, 0.1, 9, "logloss")

best_it_e05_md4 <- xgb_best_iter_bin(train_df_07, 198, 0.05, 4, "logloss")
best_it_e05_md6 <- xgb_best_iter_bin(train_df_07, 198, 0.05, 6, "logloss")
best_it_e05_md9 <- xgb_best_iter_bin(train_df_07, 198, 0.05, 9, "logloss")

best_it_e01_md4 <- xgb_best_iter_bin(train_df_07, 198, 0.01, 4, "logloss")
best_it_e01_md6 <- xgb_best_iter_bin(train_df_07, 198, 0.01, 6, "logloss")
best_it_e01_md9 <- xgb_best_iter_bin(train_df_07, 198, 0.01, 9, "logloss")

## feature importance
fst_e3_md4 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e3_md4, 0.3, 4, "logloss")
fst_e3_md6 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e3_md6, 0.3, 6, "logloss")
fst_e3_md9 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e3_md9, 0.3, 9, "logloss")

fst_e1_md4 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e1_md4, 0.1, 4, "logloss")
fst_e1_md6 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e1_md6, 0.1, 6, "logloss")
fst_e1_md9 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e1_md9, 0.1, 9, "logloss")

fst_e05_md4 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e05_md4, 0.05, 4, "logloss")
fst_e05_md6 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e05_md6, 0.05, 6, "logloss")
fst_e05_md9 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e05_md9, 0.05, 9, "logloss")

fst_e01_md4 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e01_md4, 0.01, 4, "logloss")
fst_e01_md6 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e01_md6, 0.01, 6, "logloss")
fst_e01_md9 <- xgb_fts_importance_bin(train_df_07, 198,best_it_e01_md9, 0.01, 9, "logloss")

## evaluation
fst_e3_md4[[2]]$evaluation_log[fst_e3_md4[[2]]$best_iteration,4]
fst_e3_md6[[2]]$evaluation_log[fst_e3_md6[[2]]$best_iteration,4] #best
fst_e3_md9[[2]]$evaluation_log[fst_e3_md9[[2]]$best_iteration,4]

fst_e1_md4[[2]]$evaluation_log[fst_e1_md4[[2]]$best_iteration,4]
fst_e1_md6[[2]]$evaluation_log[fst_e1_md6[[2]]$best_iteration,4]
fst_e1_md9[[2]]$evaluation_log[fst_e1_md9[[2]]$best_iteration,4]

fst_e05_md4[[2]]$evaluation_log[fst_e05_md4[[2]]$best_iteration,4]
fst_e05_md6[[2]]$evaluation_log[fst_e05_md6[[2]]$best_iteration,4]
fst_e05_md9[[2]]$evaluation_log[fst_e05_md9[[2]]$best_iteration,4]

fst_e01_md4[[2]]$evaluation_log[fst_e01_md4[[2]]$best_iteration,4]
fst_e01_md6[[2]]$evaluation_log[fst_e01_md6[[2]]$best_iteration,4]
fst_e01_md9[[2]]$evaluation_log[fst_e01_md9[[2]]$best_iteration,4] 

