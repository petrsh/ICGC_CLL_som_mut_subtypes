xgb_best_iter <- function(train_df, num_class, ncol_labels, eta, max_depth, eval_metric){
  
  xgb_params <- list(objective = "multi:softprob",
                     eval_metric = eval_metric,
                     num_class = num_class,
                     eta = eta,
                     gamma = 0,
                     max_depth = max_depth,
                     min_child_weight = 1,
                     subsample = 0.25,
                     colsample_bylevel = 1)
  xgb_matrix <- xgb.DMatrix(data = as.matrix(train_df[,-ncol_labels]),
                            label = train_df[,ncol_labels])
  model <- xgb.cv(params=xgb_params,
                  data=xgb_matrix,
                  nfold = 5,
                  nrounds = 10000,
                  early_stopping_rounds = 100,
                  nthread = 20)
  
  return(model$best_iteration)
}
