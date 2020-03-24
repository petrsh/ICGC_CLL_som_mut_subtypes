xgb_fts_importance_bin <- function(train_df, ncol_labels, n_iter, eta, max_depth, eval_metric){
  xgb_params <- list(objective = "binary:logistic",
                     eval_metric = eval_metric,
                     eta = eta,
                     gamma = 0,
                     max_depth = max_depth,
                     min_child_weight = 1,
                     subsample = 0.25,
                     colsample_bylevel = 1)
  xgb_matrix <- xgb.DMatrix(data = as.matrix(train_df[,-ncol_labels]),
                            label = train_df[,ncol_labels])
  model <- xgb.train(params=xgb_params,
                     data=xgb_matrix,
                     nrounds = n_iter,
                     nthread = 20)
  
  importance <- xgb.importance(model = model)
  
  train_df_fts <- train_df[,colnames(train_df) %in% importance$Feature]
  xgb_matrix <- xgb.DMatrix(data = as.matrix(train_df_fts),
                            label = train_df[,ncol_labels])
  model_cv <- xgb.cv(params=xgb_params,
                     data=xgb_matrix,
                     nfold = 5,
                     nrounds = 10000,
                     early_stopping_rounds = 100,
                     nthread = 20)
  
  return(list(importance,model_cv))
}