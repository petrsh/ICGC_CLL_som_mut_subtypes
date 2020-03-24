sambar_NoGeneCorrection_GmtMod <- function (mutdata = mut.ucec, signatureset = system.file("extdata",
                                                                                    "h.all.v6.1.symbols.gmt", package = "SAMBAR", mustWork = TRUE),
                                     cangenes = genes, kmin = 2, kmax = 4, ...)
{
  edg <- convertgmt_mod(signature = signatureset, cagenes = cangenes)
  #mutlength <- corgenelength(x = mutdata, cagenes = cangenes,
  #                           exonsize = esize)
  mutlength <- t(mutdata)
  patmutrate <- apply(mutlength, 2, sum)
  patmut0 <- which(patmutrate == 0)
  if (length(patmut0) > 0) {
    mutlength <- mutlength[, -patmut0, drop = F]
    patmutrate <- patmutrate[-patmut0]
  }
  mutrate <- mutlength
  for (p in 1:ncol(mutlength)) {
    mutrate[, p] <- mutlength[, p]/patmutrate[p]
  }
  mutrate <- mutrate[which(row.names(mutrate) %in% colnames(edg)),
                     ]
  genefreq <- apply(edg, 2, sum)
  genefreq <- genefreq[which(names(genefreq) %in% row.names(mutrate))]
  mutratecor <- mutrate/genefreq
  signpat <- desparsify(edgx = edg, mutratecorx = mutratecor)
  distance <- vegan::vegdist(t(signpat), method = "binomial")
  cluster <- stats::hclust(distance, method = "complete")
  groups <- list()
  cnt <- 1
  for (k in kmin:kmax) {
    groups[[cnt]] <- stats::cutree(cluster, k = k)
    cnt <- cnt + 1
  }
  names(groups) <- kmin:kmax
  return(groups)
}
