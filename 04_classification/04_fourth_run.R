set.seed(42)
train_df_07_k5_fourth <-  t_path_mut_score_mod_k5[train_07_k5, colnames(t_path_mut_score_mod_k5)  %in% c(third_fst_e05_md6[[1]]$Feature[1:84],"cluster_k5")]
test_df_07_k5_fourth <- t_path_mut_score_mod_k5[!train_07_k5, colnames(t_path_mut_score_mod_k5)  %in% c(third_fst_e05_md6[[1]]$Feature[1:84],"cluster_k5")]

fourth_best_it_e3_md4 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.3, 4, "mlogloss")
fourth_best_it_e3_md6 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.3, 6, "mlogloss")
fourth_best_it_e3_md9 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.3, 9, "mlogloss")

fourth_best_it_e1_md4 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.1, 4, "mlogloss")
fourth_best_it_e1_md6 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.1, 6, "mlogloss")
fourth_best_it_e1_md9 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.1, 9, "mlogloss")

fourth_best_it_e05_md4 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.05, 4, "mlogloss")
fourth_best_it_e05_md6 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.05, 6, "mlogloss")
fourth_best_it_e05_md9 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.05, 9, "mlogloss")

fourth_best_it_e01_md4 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.01, 4, "mlogloss")
fourth_best_it_e01_md6 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.01, 6, "mlogloss")
fourth_best_it_e01_md9 <- xgb_best_iter(train_df_07_k5_fourth,4, 85, 0.01, 9, "mlogloss")

fourth_fst_e3_md4 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e3_md4, 0.3, 4, "mlogloss")
fourth_fst_e3_md6 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e3_md6, 0.3, 6, "mlogloss")
fourth_fst_e3_md9 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e3_md9, 0.3, 9, "mlogloss")

fourth_fst_e1_md4 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e1_md4, 0.1, 4, "mlogloss")
fourth_fst_e1_md6 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e1_md6, 0.1, 6, "mlogloss")
fourth_fst_e1_md9 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e1_md9, 0.1, 9, "mlogloss")

fourth_fst_e05_md4 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e05_md4, 0.05, 4, "mlogloss")
fourth_fst_e05_md6 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e05_md6, 0.05, 6, "mlogloss")
fourth_fst_e05_md9 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e05_md9, 0.05, 9, "mlogloss")

fourth_fst_e01_md4 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e01_md4, 0.01, 4, "mlogloss")
fourth_fst_e01_md6 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e01_md6, 0.01, 6, "mlogloss")
fourth_fst_e01_md9 <- xgb_fts_importance(train_df_07_k5_fourth,4, 85,fourth_best_it_e01_md9, 0.01, 9, "mlogloss")


fourth_fst_e3_md4[[2]]$evaluation_log[fourth_fst_e3_md4[[2]]$best_iteration,4]
fourth_fst_e3_md6[[2]]$evaluation_log[fourth_fst_e3_md6[[2]]$best_iteration,4]
fourth_fst_e3_md9[[2]]$evaluation_log[fourth_fst_e3_md9[[2]]$best_iteration,4]

fourth_fst_e1_md4[[2]]$evaluation_log[fourth_fst_e1_md4[[2]]$best_iteration,4]
fourth_fst_e1_md6[[2]]$evaluation_log[fourth_fst_e1_md6[[2]]$best_iteration,4]
fourth_fst_e1_md9[[2]]$evaluation_log[fourth_fst_e1_md9[[2]]$best_iteration,4]

fourth_fst_e05_md4[[2]]$evaluation_log[fourth_fst_e05_md4[[2]]$best_iteration,4]
fourth_fst_e05_md6[[2]]$evaluation_log[fourth_fst_e05_md6[[2]]$best_iteration,4] 
fourth_fst_e05_md9[[2]]$evaluation_log[fourth_fst_e05_md9[[2]]$best_iteration,4] 

fourth_fst_e01_md4[[2]]$evaluation_log[fourth_fst_e01_md4[[2]]$best_iteration,4] #best
fourth_fst_e01_md6[[2]]$evaluation_log[fourth_fst_e01_md6[[2]]$best_iteration,4]
fourth_fst_e01_md9[[2]]$evaluation_log[fourth_fst_e01_md9[[2]]$best_iteration,4]


xgb_params_e01_md4 <- list(objective = "multi:softprob",
                           eval_metric = "mlogloss",
                           num_class = 4,
                           eta = 0.01,
                           gamma = 0,
                           max_depth = 4,
                           min_child_weight = 1,
                           subsample = 0.25,
                           colsample_bytree = 1)

train_df_07_k5_fourth_xgb <- xgb.DMatrix(data = as.matrix(train_df_07_k5_fourth[,-85]),
                                         label = train_df_07_k5_fourth[,85])

model_fourth <- xgb.train(params = xgb_params_e01_md4,
                           data = train_df_07_k5_fourth_xgb,
                           nrounds = fourth_fst_e01_md4[[2]]$best_iteration)


test_df_07_k5_fourth_xgb <- xgb.DMatrix(data = as.matrix(test_df_07_k5_fourth[,-85]),
                                        label = test_df_07_k5_fourth[,85])

pred_fourth <- predict(model_fourth, newdata=test_df_07_k5_fourth_xgb)

pred_fourth_matrix <- matrix(pred_fourth, nrow = 4,
                              ncol=length(pred_fourth)/4) %>%
                              t() %>%
                              data.frame() %>%
                              mutate(label = as.numeric(as.character(test_df_07_k5_fourth[,85])) + 1,
                                     max_prob = max.col(., "last"))


confusionMatrix(factor(pred_fourth_matrix$max_prob),
                        factor(pred_fourth_matrix$label),
                        mode = "everything")
ModelMetrics::mlogLoss(pred_fourth_matrix$label, pred_fourth_matrix[,1:4])
pred_mat <- as.matrix(pred_fourth_matrix[,1:4])
colnames(pred_mat) <- 1:4
multiclass.roc(pred_fourth_matrix$label, pred_mat)

library(multiROC)
dummy_labels <- dummies::dummy(pred_fourth_matrix$label)
pred_fourth_multi <- cbind(dummy_labels ,pred_fourth_matrix[,1:4])
colnames(pred_fourth_multi) <- c("X1_true","X2_true","X3_true","X4_true", 
                                 "X1_pred_xgb", "X2_pred_xgb", "X3_pred_xgb", "X4_pred_xgb")
multi_pr_fourth<- multi_pr(pred_fourth_multi, force_diag = T)
multi_roc(pred_fourth_multi, force_diag = T)

### Figure 2 - Feature importance
library(ggthemes)
size = 10

overall <- xgb.importance(model = model_fourth)
overall_plot <- xgb.ggplot.importance(overall,n_clusters=1, top_n = 10)

overall_plot + xlab("") +
  ylab("") +
  ggtitle("Overall") +
  theme_tufte() +
  theme(legend.position="none",text = element_text(size=size)) +
  scale_fill_manual(values="#78B1BF") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

#2,5,1,3s
imp_k5_c1 <- xgb.importance(model = model_fourth,trees = seq(from=0, by=4, length.out= fourth_fst_e01_md4[[2]]$best_iteration))
imp_k5_c2 <- xgb.importance(model = model_fourth,trees = seq(from=1, by=4, length.out= fourth_fst_e01_md4[[2]]$best_iteration))
imp_k5_c3 <- xgb.importance(model = model_fourth,trees = seq(from=2, by=4, length.out= fourth_fst_e01_md4[[2]]$best_iteration))
imp_k5_c4 <- xgb.importance(model = model_fourth,trees = seq(from=3, by=4, length.out= fourth_fst_e01_md4[[2]]$best_iteration))

imp_k5_c1$Feature <- gsub("_"," ", imp_k5_c1$Feature)
imp_k5_c2$Feature <- gsub("_"," ", imp_k5_c2$Feature)
imp_k5_c3$Feature <- gsub("_"," ", imp_k5_c3$Feature)
imp_k5_c4$Feature <- gsub("_"," ", imp_k5_c4$Feature)

c1 <- xgb.ggplot.importance(imp_k5_c1,n_clusters=1, top_n = 10)
c2 <- xgb.ggplot.importance(imp_k5_c2,n_clusters=1, top_n = 10)
c3 <- xgb.ggplot.importance(imp_k5_c3,n_clusters=1, top_n = 10)
c4 <- xgb.ggplot.importance(imp_k5_c4,n_clusters=1, top_n = 10)

size = 10
c1_x <- c1 + xlab("") +
        ylab("") +
        ggtitle("Cluster 1") +
        theme_tufte() +
        theme(legend.position="none",text = element_text(size=size)) +
        scale_fill_manual(values="#78B1BF") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
c2_x <- c2 + xlab("") +
        ylab("") +
        ggtitle("Cluster 2") +
        theme_tufte() +
        theme(legend.position="none",text = element_text(size=size)) +
        scale_fill_manual(values="#78B1BF") +
        scale_x_discrete(labels = function(x) str_wrap(x, width=30))
c3_x <- c3 + xlab("") +
        ylab("") +
        ggtitle("Cluster 3") +
        theme_tufte() +
        theme(legend.position="none",text = element_text(size=size)) +
        scale_fill_manual(values="#78B1BF") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
c4_x <- c4 + xlab("") +
        ylab("") +
        ggtitle("Cluster 4") +
        theme_tufte() +
        theme(legend.position="none",text = element_text(size=size)) +
        scale_fill_manual(values="#78B1BF") +
        scale_x_discrete(labels = function(x) str_wrap(x, width = 30))

library(grid)
library(gridExtra)
grid.arrange(c1_x,c2_x,c3_x,c4_x, left = "Pathway signatures", bottom = "Importance") 


