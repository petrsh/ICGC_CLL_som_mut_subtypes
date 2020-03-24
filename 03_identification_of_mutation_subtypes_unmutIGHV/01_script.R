library(tidyverse)
library(SAMBAR)
library(diceR)
library(survival)
library(survminer)

source("../01_identification_of_mutation_subtypes/helper_functions/convertgmt_mod.R") # Loads pathway signatures starting from the second column

### Load somatic mutation data and preprocess them
nature_df <- read.csv("../01_identification_of_mutation_subtypes/input_data/nature_data.csv", 
                      sep = ";")
nature_df <- filter(nature_df, Biotype != "miRNA")
nature_df <- filter(nature_df, !Effect %in% c("synonymous", "3pUTR", "5pUTR", "indel_3pUTR", "indel_5pUTR"))

mod_df_nat <- nature_df[,c("Case", "Symbol")] 
mod_df_nat$Binary <- rep(1, length(mod_df_nat$Case))
mod_df_nat <- group_by(mod_df_nat, Case)
mod_df_nat<-distinct(mod_df_nat, Symbol)
mod_df_nat$Binary <- rep(1, length(mod_df_nat$Case))
df_wide <- spread(mod_df_nat, Symbol, Binary)

x <- as.data.frame(df_wide[,-1], colnames=colnames(df_wide))
x[is.na(x)] <- 0
rn <- as.data.frame(df_wide[,1])
row.names(x) <- rn$Case

### Load clinical data 
donor <- read.delim("../01_identification_of_mutation_subtypes/input_data/donor.tsv")
# Load "complex metadata" from Puente,2015
compl_meta <- read.delim("../01_identification_of_mutation_subtypes/input_data/complex_metadata_2015_nature")
donor <- donor[order(donor$submitted_donor_id),]
donor <- donor[donor$submitted_donor_id %in% rownames(x),]
donor$IGHV <- compl_meta$IGHV.status

donor <- donor[-which(donor$IGHV == "Undetermined" | donor$IGHV == "BICLONAL" ),]
donor_unmut <- donor[donor$IGHV=="UNMUT",]
x_unmut <- x[row.names(x) %in% donor_unmut$submitted_donor_id,]

gene_symbols <- read.delim("../01_identification_of_mutation_subtypes/input_data/gene_symbols.txt",header=TRUE)
can_path_red_100 <- convertgmt_mod("../01_identification_of_mutation_subtypes/input_data/c2_reduced_100.gmt",
                                   gene_symbols$Approved.symbol)

### Code below is modified sambar()
###
mutlength <- t(x_unmut)
mutlength <- mutlength[which(row.names(mutlength) %in% colnames(can_path_red_100)),]
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

genefreq <- apply(can_path_red_100, 2, sum)
genefreq <- genefreq[which(names(genefreq) %in% row.names(mutrate))]
mutratecor <- mutrate/genefreq

path_mut_score <- desparsify(edgx = can_path_red_100, mutratecorx = mutratecor)
####
####
t_path_mut_score <- as.data.frame(t(path_mut_score))
table(apply(t_path_mut_score,2,function(x){sum(x>0)}) > 9)

t_path_mut_score <- t_path_mut_score[,apply(t_path_mut_score,2,function(x){sum(x>0)}) > 9]
which(apply(t_path_mut_score,1,sum) == 0)

write.csv(t_path_mut_score,"ensemble_clustering/t_path_mut_score")

cc_cp_1 <- readRDS("ensemble_clustering/cc_cp_2_4.RDS")
cc_cp_2 <- readRDS("ensemble_clustering/cc_cp_5_7.RDS")

cc_cp_1_combined <- consensus_combine(cc_cp_1,element="class")
cc_cp_2_combined <- consensus_combine(cc_cp_2,element="class")

set.seed(42)
cc_cp_1_evaluate_trimmed <- consensus_evaluate(t_path_mut_score,cc_cp_1, trim = F)
cc_cp_2_evaluate_trimmed <- consensus_evaluate(t_path_mut_score,cc_cp_2, trim = F)

apply(cc_cp_1_evaluate_trimmed$pac[,-c(1)], 1, sum)
apply(cc_cp_2_evaluate_trimmed$pac[,-c(1)], 1, sum)

#2,6,7
donor_cp <-donor[donor$submitted_donor_id %in% rownames(t_path_mut_score),]

donor_cp$cluster_k2 <- k_modes(cc_cp_1_combined$`2`)
donor_cp$cluster_k6 <- k_modes(cc_cp_2_combined$`6`)
donor_cp$cluster_k7 <- k_modes(cc_cp_2_combined$`7`)

#Relapse analysis
relapse <- rep(1,length(donor_cp$donor_relapse_interval))
relapse[is.na(donor_cp$donor_relapse_interval)] <- 0
relapse_interval <- donor_cp$donor_relapse_interval
relapse_interval[is.na(relapse_interval)] <- donor_cp[is.na(relapse_interval),"donor_interval_of_last_followup"]
relapse <- data.frame(is_relapse = relapse, relapse_interval = relapse_interval)

relapse_obj <- Surv(time = relapse$relapse_interval/365, event = relapse$is_relapse
                    , type = "right")

relapse$cluster_k2 <- donor_cp$cluster_k2
relapse$cluster_k6 <- donor_cp$cluster_k6
relapse$cluster_k7 <- donor_cp$cluster_k7

table(relapse$cluster_k2)
relapse_fit_k2 <- survfit(relapse_obj ~ cluster_k2, data=relapse)
## Figure 2 A
g1 <- ggsurvplot(relapse_fit_k2, data = relapse, pval = TRUE,
                 xlab = "Time to treatment (years)", ylab = "Probability",
                 xlim = c(0,20), legend = "right", 
                 title = "", pval.size = 4,
                 ggtheme = theme_survminer(base_family = "Helvetica",  base_size = 12, font.x = 12, font.y = 12, font.tickslab = 12),
                 fun = "event", pval.coord = c(16,0.1))

table(relapse$cluster_k6)
relapse_fit_k6 <- survfit(relapse_obj ~ cluster_k6, data=relapse)
g2 <- ggsurvplot(relapse_fit_k6, data = relapse, pval = TRUE,
                 xlab = "Time to treatment (years)", ylab = "Probability",
                 xlim = c(0,20), legend = "right", 
                 title = "", pval.size = 4,
                 ggtheme = theme_survminer(base_family = "Helvetica", base_size = 13),
                 fun = "event", pval.coord = c(16,0.1))

table(relapse$cluster_k7)
relapse_fit_k7 <- survfit(relapse_obj ~ cluster_k7, data=relapse)
g3 <- ggsurvplot(relapse_fit_k7, data = relapse, pval = TRUE,
                 xlab = "Time to treatment (years)", ylab = "Probability",
                 xlim = c(0,20), legend = "right", 
                 title = "", pval.size = 4,
                 ggtheme = theme_survminer(base_family = "Helvetica", base_size = 13),
                 fun = "event", pval.coord = c(16,0.1))
