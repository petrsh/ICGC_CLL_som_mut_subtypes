library(tidyverse)
library(SAMBAR)
library(diceR)
library(survival)
library(survminer)
library(forestmodel)

setwd("./01_identification_of_mutation_subtypes")
source("helper_functions/convertgmt_mod.R") # Loads pathway signatures starting from a second column
source("helper_functions/sambar_NoGeneCorrection_GmtMod.R")

### Load somatic mutation data and preprocess them
nature_df <- read.csv("input_data/nature_data.csv", sep = ";")
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
donor <- read.delim("input_data/donor.tsv")
# Load "complex metadata" from Puente,2015
compl_meta <- read.delim("input_data/complex_metadata_2015_nature")
donor <- donor[order(donor$submitted_donor_id),]
donor <- donor[donor$submitted_donor_id %in% rownames(x),]
donor$IGHV <- compl_meta$IGHV.status

gene_symbols <- read.delim("input_data/gene_symbols.txt",header=TRUE)

### SAMBAR with subsetting to cancer associated genes and without gene lenght correction
sambar <- sambar_NoGeneCorrection_GmtMod(mutdata=x,signatureset="input_data/c2_reduced_100.gmt",
                                         kmin=2,kmax=7)
### SAMBAR without subsetting to cancer associated genes and without gene lenght correction
sambar_ngc <-  sambar_NoGeneCorrection_GmtMod(mutdata=x,signatureset="input_data/c2_reduced_100.gmt",
                                              kmin=2,kmax=7, cangenes = gene_symbols$Approved.symbol)

### TTFT analysis
donor_samb <- donor[donor$submitted_donor_id %in% names(sambar_ngc$`2`),]
donor_samb$k3 <- sambar_ngc$`3`
donor_samb$k5 <- sambar_ngc$`5`

donor_samb_k3 <- donor_samb[donor_samb$k3!=3,]
relapse_k3 <- rep(1,length(donor_samb_k3$donor_relapse_interval))
relapse_k3[is.na(donor_samb_k3$donor_relapse_interval)] <- 0
relapse_interval <- donor_samb_k3$donor_relapse_interval
relapse_interval[is.na(relapse_interval)] <- donor_samb_k3[is.na(relapse_interval),"donor_interval_of_last_followup"]
relapse_k3 <- data.frame(is_relapse_k3 = relapse_k3, relapse_interval = relapse_interval)
relapse_k3$k3 <- donor_samb_k3$k3
relapse_obj_k3 <- Surv(time = relapse_k3$relapse_interval/365, event = relapse_k3$is_relapse, type = "right")
survdiff(relapse_obj_k3 ~ k3, data=relapse_k3)

donor_samb_k5 <- donor_samb[donor_samb$k5 %in% c(1,2),]
relapse_k5 <- rep(1,length(donor_samb_k5$donor_relapse_interval))
relapse_k5[is.na(donor_samb_k5$donor_relapse_interval)] <- 0
relapse_interval <- donor_samb_k5$donor_relapse_interval
relapse_interval[is.na(relapse_interval)] <- donor_samb_k5[is.na(relapse_interval),"donor_interval_of_last_followup"]
relapse_k5 <- data.frame(is_relapse_k5 = relapse_k5, relapse_interval = relapse_interval)
relapse_k5$k5 <- donor_samb_k5$k5
relapse_obj_k5 <- Surv(time = relapse_k5$relapse_interval/365, event = relapse_k5$is_relapse, type = "right")
survdiff(relapse_obj_k5 ~ k5, data=relapse_k5)

### Load reduced signature dataset
can_path_red_100 <- convertgmt_mod("input_data/c2_reduced_100.gmt", gene_symbols$Approved.symbol)
### Code below is modified sambar() from SAMBAR package
mutlength <- t(x)
mutlength <- mutlength[which(row.names(mutlength) %in% colnames(can_path_red_100)),] # subsets to genes in pathways before normalization
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

# Remove low occurring signatures
t_path_mut_score <- t_path_mut_score[,apply(t_path_mut_score,2,function(x){sum(x>0)}) > 9]
which(apply(t_path_mut_score,1,sum) == 0)
t_path_mut_score <- t_path_mut_score[-84,]

write.csv(t_path_mut_score,"ensemble_clustering/t_path_mut_score")
### Ensemble clustering was performed with the following scripts in the ~/01_identification_of_mutation_subtypes/ensemble_clustering/
#script_to_run1.R
#script_to_run2.R

# Load the results of the ensemble clustering
cc_cp_1_NORM <- readRDS("ensemble_clustering/cc_cp_2_4.RDS")
cc_cp_2_NORM <- readRDS("ensemble_clustering/cc_cp_5_7.RDS")

cc_cp_1_combined_NORM <- consensus_combine(cc_cp_1_NORM,element="class")
cc_cp_2_combined_NORM <- consensus_combine(cc_cp_2_NORM,element="class")

set.seed(42)
cc_cp_1_evaluate_trimmed_NORM <- consensus_evaluate(t_path_mut_score,cc_cp_1_NORM, trim = F)
cc_cp_2_evaluate_trimmed_NORM <- consensus_evaluate(t_path_mut_score,cc_cp_2_NORM, trim = F)

apply(cc_cp_1_evaluate_trimmed_NORM$pac[,-c(1)], 1, sum)
apply(cc_cp_2_evaluate_trimmed_NORM$pac[,-c(1)], 1, sum)

donor_cp <-donor[donor$submitted_donor_id %in% rownames(t_path_mut_score),]

donor_cp$cluster_k2 <- k_modes(cc_cp_1_combined_NORM$`2`)
donor_cp$cluster_k5 <- k_modes(cc_cp_2_combined_NORM$`5`)
donor_cp$cluster_k7 <- k_modes(cc_cp_2_combined_NORM$`7`)

### TTFT analysis
relapse <- rep(1,length(donor_cp$donor_relapse_interval))
relapse[is.na(donor_cp$donor_relapse_interval)] <- 0
relapse_interval <- donor_cp$donor_relapse_interval
relapse_interval[is.na(relapse_interval)] <- donor_cp[is.na(relapse_interval),"donor_interval_of_last_followup"]
relapse <- data.frame(is_relapse = relapse, relapse_interval = relapse_interval)

relapse_obj <- Surv(time = relapse$relapse_interval/365, event = relapse$is_relapse
                    , type = "right")

relapse$cluster_k2 <- donor_cp$cluster_k2
relapse$cluster_k5 <- donor_cp$cluster_k5
relapse$cluster_k7 <- donor_cp$cluster_k7

relapse_fit_k2 <- survfit(relapse_obj ~ cluster_k2, data=relapse)
g1 <- ggsurvplot(relapse_fit_k2, data = relapse, pval = TRUE,
                 xlab = "Time to treatment (years)", ylab = "Probability",
                 xlim = c(0,20), legend = "none", 
                 title = "", pval.size = 4,
                 ggtheme = theme_survminer(base_family = "Helvetica",  base_size = 12, font.x = 12, font.y = 12, font.tickslab = 12),
                 fun = "event", pval.coord = c(16,0.1))

relapse_mod_k5 <- relapse[relapse$cluster_k5 !=4, ]
relapse_obj_mod_k5 <- Surv(time = relapse_mod_k5$relapse_interval/365, event = relapse_mod_k5$is_relapse
                          ,type = "right")
relapse_fit_k5 <- survfit(relapse_obj_mod_k5 ~ cluster_k5, data=relapse_mod_k5)
g2  <- ggsurvplot(relapse_fit_k5, data = relapse_mod_k5, pval = TRUE,
                  xlab = "Time to first treatment (years)", ylab = "Probability",
                  xlim = c(0,20), legend ="right", legend.title = "",
                  title = "", pval.size = 4,
                  ggtheme = theme_survminer(base_family = "Helvetica",  base_size = 12, font.x = 12, font.y = 12, font.tickslab = 12),
                  fun = "event", pval.coord = c(16,0.1))

relapse_fit_k7 <- survfit(relapse_obj ~ cluster_k7, data=relapse)
g3 <- ggsurvplot(relapse_fit_k7, data = relapse, pval = TRUE,
                 xlab = "Time to treatment (years)", ylab = "Probability",
                 xlim = c(0,20), legend = "right", 
                 title = "", pval.size = 4,
                 ggtheme = theme_survminer(base_family = "Helvetica",  base_size = 12, font.x = 12, font.y = 12, font.tickslab = 12),
                 fun = "event", pval.coord = c(16,0.1))

####
####
donor_cp_IGHV <- donor_cp[-which(donor_cp$IGHV == "Undetermined" | donor_cp$IGHV == "BICLONAL" ),]

relapse_IGHV <- rep(1,length(donor_cp_IGHV$donor_relapse_interval))
relapse_IGHV[is.na(donor_cp_IGHV$donor_relapse_interval)] <- 0
relapse_IGHV_interval <- donor_cp_IGHV$donor_relapse_interval
relapse_IGHV_interval[is.na(relapse_IGHV_interval)] <- donor_cp_IGHV[is.na(relapse_IGHV_interval),"donor_interval_of_last_followup"]
relapse_IGHV <- data.frame(is_relapse_IGHV = relapse_IGHV, relapse_IGHV_interval = relapse_IGHV_interval)

donor_cp_IGHV$IGHV <- as.factor(as.character(donor_cp_IGHV$IGHV))
relapse_IGHV$cluster_k5 <- as.factor(donor_cp_IGHV$cluster_k5)
relapse_IGHV$IGHV <- donor_cp_IGHV$IGHV

relapse_IGHV_mod_k5 <- relapse_IGHV[relapse_IGHV$cluster_k5!=4,]
relapse_IGHV_mod_k5[relapse_IGHV_mod_k5$cluster_k5=="5","cluster_k5"] <- 4
relapse_IGHV_mod_k5$cluster_k5 <- as.factor(as.character(relapse_IGHV_mod_k5$cluster_k5))
relapse_IGHV_mod_k5$cluster_k5 <- factor(relapse_IGHV_mod_k5$cluster_k5, levels = c("2","4","1","3"))
relapse_IGHV_mod_k5$cluster <- relapse_IGHV_mod_k5$cluster_k5
relapse_IGHV_mod_k5$IGHV <- factor(relapse_IGHV_mod_k5$IGHV, levels = c("UNMUT","MUT"))
relapse_IGHV_obj_mod_k5 <- Surv(time = relapse_IGHV_mod_k5$relapse_IGHV_interval/365, event = relapse_IGHV_mod_k5$is_relapse_IGHV
                                , type = "right")
coxph_relapse_fit_k5 <- coxph(relapse_IGHV_obj_mod_k5 ~ cluster + IGHV, data=relapse_IGHV_mod_k5)
fm_g_rel_k5 <- forest_model(coxph_relapse_fit_k5, format_options = list(text_size=3.6))

### Survival analysis
donor_surv <- donor_cp[-which(donor_cp$donor_vital_status == ""),]

donor_surv$donor_vital_status <- as.character(donor_surv$donor_vital_status)
donor_surv[donor_surv$donor_vital_status == "alive", "donor_vital_status"] <- 0
donor_surv[donor_surv$donor_vital_status == "deceased", "donor_vital_status"] <- 1
donor_surv$donor_vital_status <- as.numeric(donor_surv$donor_vital_status)

donor_surv_mod_k5 <- donor_surv[donor_surv$cluster_k5 != 4, ]
#
surv_obj_mod_k5 <- Surv(time = donor_surv_mod_k5$donor_survival_time/365, event = donor_surv_mod_k5$donor_vital_status, type = "right")
#k5
surv_fit_k5 <- survfit(surv_obj_mod_k5 ~ cluster_k5, data=donor_surv_mod_k5)
ggsurv_k5 <- ggsurvplot(surv_fit_k5, data = donor_surv_mod_k5, pval=TRUE, xlab = "Years", 
                         xlim = c(0,20),  legend = "right", 
                         ggtheme = theme_survminer(base_family = "Helvetica",  base_size = 12, font.x = 12, font.y = 12, font.tickslab = 12),
                         pval.coord = c(16,0.1), pval.size = 4)
#####
donor_surv_IGHV <- donor_surv[-which(donor_surv$IGHV == "Undetermined" | donor_surv$IGHV == "BICLONAL" ),]
donor_surv_IGHV$IGHV <- as.factor(as.character(donor_surv_IGHV$IGHV))
donor_surv_IGHV_mod_k5 <- donor_surv_IGHV[donor_surv_IGHV$cluster_k5 != 4, ]
donor_surv_IGHV_mod_k5$cluster <- as.character(donor_surv_IGHV_mod_k5$cluster_k5)
donor_surv_IGHV_mod_k5[donor_surv_IGHV_mod_k5$cluster=="5","cluster"] <- 4
donor_surv_IGHV_mod_k5$cluster <- as.factor(as.character(donor_surv_IGHV_mod_k5$cluster))
donor_surv_IGHV_mod_k5$cluster <- factor(donor_surv_IGHV_mod_k5$cluster, levels = c("2","4","1","3"))
donor_surv_IGHV_mod_k5$IGHV <- factor(donor_surv_IGHV_mod_k5$IGHV, levels = c("UNMUT","MUT"))

surv_obj_IGHV_mod_k5 <- Surv(time = donor_surv_IGHV_mod_k5$donor_survival_time/365, 
                             event = donor_surv_IGHV_mod_k5$donor_vital_status, type = "right")
coxph_surv_fit_k5 <- coxph(surv_obj_IGHV_mod_k5 ~ cluster + IGHV, data=donor_surv_IGHV_mod_k5)

### Figure 1
splots <- list()
splots[[1]] <- g2
splots[[2]] <- ggsurv_k5

arrange_ggsurvplots(splots, ncol=1, nrow = 2)

fm_plot <- forest_model(coxph_surv_fit_k5, format_options = list(text_size=3.6))
ggarrange(fm_g_rel_k5,fm_plot, ncol = 1, nrow = 2)
