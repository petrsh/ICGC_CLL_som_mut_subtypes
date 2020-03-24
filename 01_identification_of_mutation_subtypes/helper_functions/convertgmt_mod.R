convertgmt_mod <- function (signature, cagenes, ...) 
{
  ncols <- scan(signature, what = "character")
  ncols <- length(unique(ncols))
  sign <- utils::read.table(signature, header = FALSE, sep = "\t", 
                            col.names = paste0("V", seq_len(ncols)), fill = TRUE)
  row.names(sign) <- sign[, 1]
  #sign <- sign[, 3:ncol(sign)]
  sign <- sign[, 2:ncol(sign)]
  sign <- as.matrix(sign)
  genefreq <- c(sign)
  genefreq <- table(genefreq)
  namesgenefreq <- names(genefreq)
  genefreq <- as.vector(genefreq)
  names(genefreq) <- namesgenefreq
  genefreq <- genefreq[-1]
  allgenes <- unique(unlist(c(sign)))
  allgenes <- allgenes[order(allgenes)]
  allgenes <- allgenes[which(allgenes != "")]
  signmat <- matrix(0, nrow(sign), length(allgenes))
  row.names(signmat) <- row.names(sign)
  colnames(signmat) <- allgenes
  for (i in 1:nrow(sign)) {
    signmat[i, which(allgenes %in% sign[i, ])] <- 1
  }
  signmat <- signmat[, which(colnames(signmat) %in% cagenes)]
  signmat <- signmat[which(apply(signmat, 1, sum) > 0), ]
  signmat <- signmat[, which(apply(signmat, 2, sum) > 0)]
  return(signmat)
}
