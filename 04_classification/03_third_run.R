set.seed(42)
train_df_07_k5_third <-  t_path_mut_score_mod_k5[train_07_k5, colnames(t_path_mut_score_mod_k5)  %in% c(sec_fst_e05_md9[[1]]$Feature[1:112],"cluster_k5")]
test_df_07_k5_third <- t_path_mut_score_mod_k5[!train_07_k5, colnames(t_path_mut_score_mod_k5)  %in% c(sec_fst_e05_md9[[1]]$Feature[1:112],"cluster_k5")]

third_best_it_e3_md4 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.3, 4, "mlogloss")
third_best_it_e3_md6 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.3, 6, "mlogloss")
third_best_it_e3_md9 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.3, 9, "mlogloss")

third_best_it_e1_md4 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.1, 4, "mlogloss")
third_best_it_e1_md6 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.1, 6, "mlogloss")
third_best_it_e1_md9 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.1, 9, "mlogloss")

third_best_it_e05_md4 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.05, 4, "mlogloss")
third_best_it_e05_md6 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.05, 6, "mlogloss")
third_best_it_e05_md9 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.05, 9, "mlogloss")

third_best_it_e01_md4 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.01, 4, "mlogloss")
third_best_it_e01_md6 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.01, 6, "mlogloss")
third_best_it_e01_md9 <- xgb_best_iter(train_df_07_k5_third,4, 113, 0.01, 9, "mlogloss")

third_fst_e3_md4 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e3_md4, 0.3, 4, "mlogloss")
third_fst_e3_md6 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e3_md6, 0.3, 6, "mlogloss")
third_fst_e3_md9 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e3_md9, 0.3, 9, "mlogloss")

third_fst_e1_md4 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e1_md4, 0.1, 4, "mlogloss")
third_fst_e1_md6 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e1_md6, 0.1, 6, "mlogloss")
third_fst_e1_md9 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e1_md9, 0.1, 9, "mlogloss")

third_fst_e05_md4 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e05_md4, 0.05, 4, "mlogloss")
third_fst_e05_md6 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e05_md6, 0.05, 6, "mlogloss")
third_fst_e05_md9 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e05_md9, 0.05, 9, "mlogloss")

third_fst_e01_md4 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e01_md4, 0.01, 4, "mlogloss")
third_fst_e01_md6 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e01_md6, 0.01, 6, "mlogloss")
third_fst_e01_md9 <- xgb_fts_importance(train_df_07_k5_third,4, 113,third_best_it_e01_md9, 0.01, 9, "mlogloss")

third_fst_e3_md4[[2]]$evaluation_log[third_fst_e3_md4[[2]]$best_iteration,4]
third_fst_e3_md6[[2]]$evaluation_log[third_fst_e3_md6[[2]]$best_iteration,4]
third_fst_e3_md9[[2]]$evaluation_log[third_fst_e3_md9[[2]]$best_iteration,4]

third_fst_e1_md4[[2]]$evaluation_log[third_fst_e1_md4[[2]]$best_iteration,4]
third_fst_e1_md6[[2]]$evaluation_log[third_fst_e1_md6[[2]]$best_iteration,4]
third_fst_e1_md9[[2]]$evaluation_log[third_fst_e1_md9[[2]]$best_iteration,4]

third_fst_e05_md4[[2]]$evaluation_log[third_fst_e05_md4[[2]]$best_iteration,4]
third_fst_e05_md6[[2]]$evaluation_log[third_fst_e05_md6[[2]]$best_iteration,4] #best
third_fst_e05_md9[[2]]$evaluation_log[third_fst_e05_md9[[2]]$best_iteration,4] 

third_fst_e01_md4[[2]]$evaluation_log[third_fst_e01_md4[[2]]$best_iteration,4]
third_fst_e01_md6[[2]]$evaluation_log[third_fst_e01_md6[[2]]$best_iteration,4]
third_fst_e01_md9[[2]]$evaluation_log[third_fst_e01_md9[[2]]$best_iteration,4]
