train_df_07_sec <-  t_path_mut_score[train_07, colnames(t_path_mut_score)  %in% c(fst_e3_md6[[1]]$Feature[1:35],"cluster_k2")]
test_df_07_sec <- t_path_mut_score[!train_07, colnames(t_path_mut_score)  %in% c(fst_e3_md6[[1]]$Feature[1:35],"cluster_k2")]


sec_best_it_e3_md4 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.3, 4, "logloss")
sec_best_it_e3_md6 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.3, 6, "logloss")
sec_best_it_e3_md9 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.3, 9, "logloss")

sec_best_it_e1_md4 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.1, 4, "logloss")
sec_best_it_e1_md6 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.1, 6, "logloss")
sec_best_it_e1_md9 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.1, 9, "logloss")

sec_best_it_e05_md4 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.05, 4, "logloss")
sec_best_it_e05_md6 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.05, 6, "logloss")
sec_best_it_e05_md9 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.05, 9, "logloss")

sec_best_it_e01_md4 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.01, 4, "logloss")
sec_best_it_e01_md6 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.01, 6, "logloss")
sec_best_it_e01_md9 <- xgb_best_iter_bin(train_df_07_sec, 36, 0.01, 9, "logloss")

sec_fst_e3_md4 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e3_md4, 0.3, 4, "logloss")
sec_fst_e3_md6 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e3_md6, 0.3, 6, "logloss")
sec_fst_e3_md9 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e3_md9, 0.3, 9, "logloss")

sec_fst_e1_md4 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e1_md4, 0.1, 4, "logloss")
sec_fst_e1_md6 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e1_md6, 0.1, 6, "logloss")
sec_fst_e1_md9 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e1_md9, 0.1, 9, "logloss")

sec_fst_e05_md4 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e05_md4, 0.05, 4, "logloss")
sec_fst_e05_md6 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e05_md6, 0.05, 6, "logloss")
sec_fst_e05_md9 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e05_md9, 0.05, 9, "logloss")

sec_fst_e01_md4 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e01_md4, 0.01, 4, "logloss")
sec_fst_e01_md6 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e01_md6, 0.01, 6, "logloss")
sec_fst_e01_md9 <- xgb_fts_importance_bin(train_df_07_sec, 36,sec_best_it_e01_md9, 0.01, 9, "logloss")


sec_fst_e3_md4[[2]]$evaluation_log[sec_fst_e3_md4[[2]]$best_iteration,4]
sec_fst_e3_md6[[2]]$evaluation_log[sec_fst_e3_md6[[2]]$best_iteration,4]
sec_fst_e3_md9[[2]]$evaluation_log[sec_fst_e3_md9[[2]]$best_iteration,4]

sec_fst_e1_md4[[2]]$evaluation_log[sec_fst_e1_md4[[2]]$best_iteration,4]
sec_fst_e1_md6[[2]]$evaluation_log[sec_fst_e1_md6[[2]]$best_iteration,4]
sec_fst_e1_md9[[2]]$evaluation_log[sec_fst_e1_md9[[2]]$best_iteration,4]

sec_fst_e05_md4[[2]]$evaluation_log[sec_fst_e05_md4[[2]]$best_iteration,4]
sec_fst_e05_md6[[2]]$evaluation_log[sec_fst_e05_md6[[2]]$best_iteration,4] #best
sec_fst_e05_md9[[2]]$evaluation_log[sec_fst_e05_md9[[2]]$best_iteration,4]

sec_fst_e01_md4[[2]]$evaluation_log[sec_fst_e01_md4[[2]]$best_iteration,4]
sec_fst_e01_md6[[2]]$evaluation_log[sec_fst_e01_md6[[2]]$best_iteration,4]
sec_fst_e01_md9[[2]]$evaluation_log[sec_fst_e01_md9[[2]]$best_iteration,4]

sec_fst_e05_md6
xgb_params_e05_md6 <- list(objective = "binary:logistic",
                          eval_metric = "logloss",
                          eta = 0.05,
                          gamma = 0,
                          max_depth = 6,
                          min_child_weight = 1,
                          subsample = 0.25,
                          colsample_bytree = 1)

train_df_07_xgb <- xgb.DMatrix(data = as.matrix(train_df_07[,sec_fst_e05_md6[[1]]$Feature]),
                               label = train_df_07$cluster_k2)

model <- xgb.train(params = xgb_params_e05_md6,
                   data = train_df_07_xgb,
                   nrounds = fst_e05_md6[[2]]$best_iteration)

test_df_07_xgb <- xgb.DMatrix(data = as.matrix(test_df_07[,sec_fst_e05_md6[[1]]$Feature]),
                              label = test_df_07$cluster_k2)
pred <- predict(model, newdata=test_df_07_xgb)
confusionMatrix(as.factor(as.numeric(pred > 0.5)),as.factor(test_df_07$cluster_k2))

library(ROCR)
ROCR::performance(prediction(pred,as.factor(test_df_07$cluster_k2)), "auc")

library(PRROC)
plot(pr.curve(scores.class0=pred,
              weights.class0=test_df_07$cluster_k2,curve=T))
###
### Figure 2 B
imp_gg <- xgb.ggplot.importance(xgb.importance(model=model),left_margin = 15, top_n=10)
size = 10
library(ggthemes)
imp_gg_x <- imp_gg + xlab("") +
    ylab("") +
    ggtitle("") +
    theme_tufte() +
    theme(legend.position="none",text = element_text(size=size)) +
    scale_fill_manual(values="#78B1BF") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

