#!/usr/bin/env Rscript
library(diceR)
rf <- function(x) {
  tmp <- randomForest::randomForest(x = x, y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE)
  stats::as.dist(1 - tmp$proximity)
}
#custom binomial function
binomial <- function(x) {
  vegan::vegdist(x, method = "binomial")
}
#custom mahalanobis function
mahalanobis <- function(x) {
  vegan::vegdist(x, method = "mahalanobis")
}
t_cp_desp_flt <- read.csv("./t_path_mut_score", row.names = 1)
cc_cp <- consensus_cluster(t_cp_desp_flt, nk=5:7, reps = 5, algorithms = c("hc","diana", "pam", "km","som","sc"),
                           hc.method = "ward.D2", p.item = 0.9, distance = c("rf", "binomial", "mahalanobis"))
saveRDS(cc_cp, "cc_cp_5_7.RDS")