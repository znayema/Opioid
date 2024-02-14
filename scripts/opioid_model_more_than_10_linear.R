# GOAL: Build individual analytic file

# DATA --------------------------------------------------------------------
# 
# Read all relevant data sets.
#
# *************************************************************************
rm(list=ls())
phenoPath = "/work/larylab/dbgap/"
library(dplyr)
library(tidyverse)
setwd(phenoPath)
common_cols <- c("DBGAP_SUBJECT_ID", "SHAREID", "IDTYPE")

#dates
examdate1 <- 'data9/phs000007.v33.pht003099.v8.p14.c1.vr_dates_2019_a_1175s.HMB-IRB-MDS.txt.gz'
examdate2 <-'data9/phs000007.v33.pht003099.v8.p14.c2.vr_dates_2019_a_1175s.HMB-IRB-NPU-MDS.txt.gz'

#covs
covoff1 <- 'data9/phs000007.v33.pht006027.v4.p14.c1.vr_wkthru_ex09_1_1001s.HMB-IRB-MDS.txt.gz'
covoff2 <-'data9/phs000007.v33.pht006027.v4.p14.c2.vr_wkthru_ex09_1_1001s.HMB-IRB-NPU-MDS.txt.gz'

covgen1<-'data9/phs000007.v33.pht006026.v5.p14.c1.vr_wkthru_ex03_3b_1191s.HMB-IRB-MDS.txt.gz'
covgen2<-'data9/phs000007.v33.pht006026.v5.p14.c2.vr_wkthru_ex03_3b_1191s.HMB-IRB-NPU-MDS.txt.gz'


#drug
drugoff1 <- 'data9/phs000007.v33.pht000828.v8.p14.c1.meds1_8s.HMB-IRB-MDS.txt.gz'
drugoff2<-'data9/phs000007.v33.pht000828.v8.p14.c2.meds1_8s.HMB-IRB-NPU-MDS.txt.gz'

druggen1<- 'data9/phs000007.v33.pht003098.v6.p14.c1.vr_meds_2011_m_0675s.HMB-IRB-MDS.txt.gz'
druggen2<-'data9/phs000007.v33.pht003098.v6.p14.c2.vr_meds_2011_m_0675s.HMB-IRB-NPU-MDS.txt.gz'

#miRNA
miRNA1<-'data2/phe000005.v9.FHS_SABRe_project4_miRNA.expression-data-matrixfmt.c1/l_mrna_2011_m_0797s_17_c1.csv.gz'
miRNA2<-'data2/phe000005.v9.FHS_SABRe_project4_miRNA.expression-data-matrixfmt.c2/l_mrna_2011_m_0797s_17_c2.csv.gz'




# BMD
bmd1_off8 <- '/data9/phs000007.v33.pht003096.v4.p14.c1.t_bmdhs_2008_1_0748s.HMB-IRB-MDS.txt.gz'
bmd2_off8 <- '/data9/phs000007.v33.pht003096.v4.p14.c2.t_bmdhs_2008_1_0748s.HMB-IRB-NPU-MDS.txt.gz'
bmd11_gen3 <- '/data9/phs000007.v33.pht001182.v8.p14.c1.t_bmdhs_2009_3_0627s.HMB-IRB-MDS.txt.gz'
bmd12_gen3 <- '/data9/phs000007.v33.pht001182.v8.p14.c2.t_bmdhs_2009_3_0627s.HMB-IRB-NPU-MDS.txt.gz'
bmd21_gen3 <- '/data9/phs000007.v33.pht001892.v5.p14.c1.t_bmdhs_2010_3_0626s.HMB-IRB-MDS.txt.gz'
bmd22_gen3 <- '/data9/phs000007.v33.pht001892.v5.p14.c2.t_bmdhs_2010_3_0626s.HMB-IRB-NPU-MDS.txt.gz'
bmd31_gen3 <- '/data9/phs000007.v33.pht002346.v3.p14.c1.t_bmdhs_2011_3_0625s.HMB-IRB-MDS.txt.gz'
bmd32_gen3 <- '/data9/phs000007.v33.pht002346.v3.p14.c2.t_bmdhs_2011_3_0625s.HMB-IRB-NPU-MDS.txt.gz'



mainPath = "/work/larylab/NAYEMA/"
setwd(mainPath)
out_dir = "output"
data_dir = phenoPath
# read in data
data_labels <- c('examdate1','examdate2','covoff1','covoff2','covgen1','covgen2','drugoff1','drugoff2',
                 'druggen1','druggen2','miRNA1','miRNA2', 'bmd1_off8','bmd2_off8','bmd11_gen3','bmd12_gen3',
                 'bmd21_gen3','bmd22_gen3','bmd31_gen3','bmd32_gen3')
data_list <- lapply(setNames(data_labels, data_labels), function(i) {
  print(i)
  # NOTE: Use fill = T primarily for off_iac where certain rows have fewer columns than the rest of the dataList frame;
  #   the fill argument just adds blank fields so that every row has the same number of columns.
  output <-
    read.table(gzfile(paste(data_dir, get(i), sep = '/')),
               sep = '\t',
               fill = T,
               header = T)
  # Convert all column names to uppercase for consistency.
  names(output) <- toupper(names(output))
  output
})
output_dir <- "/work/larylab/NAYEMA/output"


saveRDS(data_list, paste(out_dir, 'datalist.rds', sep = '/'))



#dates
exam_dates <- bind_rows(data_list[['examdate1']], data_list[['examdate2']]) %>%
  dplyr::select(matches(common_cols),AGE1,SEX,ATT2,ATT8,DATE2,DATE8,AGE2,AGE8) 

#covs
covoff <-bind_rows(data_list[['covoff1']], data_list[['covoff2']]) %>% 
  dplyr::select(matches(c(common_cols,'SEX','ATT8','AGE1','AGE8','BG8','BMI8','CALC_LDL8','CURRSMK8','DBP8',
                          'FASTING_BG8','CPD8','DMRX8','HRX8','LIPRX8','IDTYPE'))) %>%
  left_join(exam_dates)%>%rename(BG = BG8,BMI=BMI8,LDL=CALC_LDL8,CURRSMK=CURRSMK8,DBP=DBP8,FASTING_BG=FASTING_BG8,CPD=CPD8,DMRX=DMRX8,HRX=HRX8,
                                 LIPRX=LIPRX8)%>%
  mutate(EXAM_DATE = DATE8) %>%
  mutate(AGE = AGE1 + EXAM_DATE/365.25) %>%
  filter(!is.na(EXAM_DATE)) %>%
  dplyr::select(matches(common_cols),AGE,SEX,EXAM_DATE,BMI,BG,LDL,CURRSMK,CPD,DMRX,HRX)



covgen<-bind_rows(data_list[['covgen1']], data_list[['covgen2']])%>% 
  dplyr::select(matches(c(common_cols,'SEX','ATT2','AGE1','AGE2','BG2','BMI2','CALC_LDL2','CURRSMK2','DBP2',
                          'FASTING_BG2','CPD2','DMRX2','HRX2','LIPRX2','IDTYPE'))) %>%
  left_join(exam_dates)%>%rename(BG = BG2,BMI=BMI2,LDL=CALC_LDL2,CURRSMK=CURRSMK2,DBP=DBP2,FASTING_BG=FASTING_BG2,CPD=CPD2,DMRX=DMRX2,HRX=HRX2,
                                 LIPRX=LIPRX2)%>%
  mutate(EXAM_DATE = DATE2) %>%
  mutate(AGE = AGE1 + EXAM_DATE/365.25) %>%
  filter(!is.na(EXAM_DATE)) %>%
  dplyr::select(matches(common_cols),AGE,SEX,EXAM_DATE,BMI,BG,LDL,CURRSMK,CPD,DMRX,HRX)

covoff<-covoff
covgen<-covgen
all_cov <- bind_rows(covoff,covgen)


#drugs
druggen<-bind_rows(data_list[['druggen1']], data_list[['drugogen2']])%>%
  dplyr::select(matches(c(common_cols,'IDTYPE','ATC_COD1','SYSTEM1','THER_GP1','THER_GP2','PHRM_GP1','CHEM_GP1','CHEM_NM1','MEDNAME','MEDPER','MEDPRN'))) 
drugoff<-bind_rows(data_list[['drugoff1']], data_list[['drugoff2']])%>%
  dplyr::select(matches(c(common_cols,'IDTYPE','ATC_COD1','SYSTEM1','THER_GP1','THER_GP2','PHRM_GP1','CHEM_GP1','CHEM_NM1','MEDNAME','MEDPER','MEDPRN'))) 

all_drug<-bind_rows(drugoff,druggen)

OPIOID_drug<-bind_rows(drugoff,druggen)%>% 
  select(SHAREID,PHRM_GP1,THER_GP1,MEDNAME,MEDPRN,MEDPER,CHEM_GP1,CHEM_NM1) %>% 
  filter(PHRM_GP1=="OPIOIDS") %>% 
  distinct(SHAREID, .keep_all = TRUE)
#bmd
bmd_gen3 <- bind_rows(data_list[['bmd11_gen3']],data_list[['bmd12_gen3']],data_list[['bmd21_gen3']],data_list[['bmd22_gen3']],
                      data_list[['bmd31_gen3']],data_list[['bmd32_gen3']]) %>%
  dplyr::select(matches(c(common_cols,'F2SCDT','F2NBMD'))) %>%
  left_join(exam_dates) %>%
  rename(FNBMD = F2NBMD) %>%
  filter(!is.na(FNBMD)) %>%
  mutate(BMD_DATE = F2SCDT) %>%
  mutate(BMD_AGE = AGE1 + BMD_DATE/365.25) %>%
  filter(!is.na(BMD_DATE)) %>%
  dplyr::select(matches(common_cols),FNBMD,BMD_DATE,BMD_AGE,SEX)

library(tidyr)


bmd_off8 <- bind_rows(data_list[['bmd1_off8']], data_list[['bmd2_off8']]) %>%
  dplyr::select(matches(common_cols), F8CBNBMD,F8CBSCDT) %>% 
  rename(FNBMD = F8CBNBMD) %>% 
  #pivot_longer(c(F8CBNBMD), names_to = "EXAM", values_to = "FNBMD") %>%
  left_join(exam_dates) %>%
  filter(!is.na(FNBMD)) %>%
  mutate(BMD_DATE =F8CBSCDT) %>%
  mutate(BMD_AGE = AGE1 + BMD_DATE/365.25) %>%
  filter(!is.na(BMD_DATE)) %>%
  dplyr::select(matches(common_cols),FNBMD,BMD_DATE,BMD_AGE,SEX)


all_bmd <- bind_rows(bmd_off8,bmd_gen3) %>%
  mutate(MEASURE = 'BMD') %>%
  mutate(VALUE = FNBMD) %>%
  mutate(AGE = BMD_AGE) %>%
  mutate(DATE = BMD_DATE) %>%
  dplyr::select(matches(common_cols),MEASURE,VALUE,DATE,AGE,SEX)











#miRNA
miRNAdat <- bind_rows(
  miRNA1 = data_list[['miRNA1']],
  miRNA2 = data_list[['miRNA2']]
)



colnames(miRNAdat) <- gsub("\\.", ",", colnames(miRNAdat))

miRNAdat2 <- miRNAdat  %>%
  separate(colnames(miRNAdat ), into = paste("Column", 1:416), sep = ",", remove = TRUE, convert = TRUE)



# Assuming split_column_names is a list of character vectors
split_column_names <- strsplit(colnames(miRNAdat), "\\,")

# Create a data frame with split names as rows
split_df <- as.data.frame(do.call(rbind, split_column_names))

colnames(miRNAdat2) <- colnames(split_df)
colnames(miRNAdat2) <- split_df[1, ]



miRNA_delta_cq <- miRNAdat2[-1]
miRNA_delta_cq <- -(miRNA_delta_cq-27)
miRNAdat2 <- cbind(miRNAdat2[1], miRNA_delta_cq)
# Merge Phenotype data together ####

cohort <- merge(exam_dates,miRNAdat2,by.x="SHAREID",by.y="SHAREID",all.x=T)%>%filter(SHAREID %in% miRNAdat2$SHAREID) 
cohort <- merge(cohort,all_cov,by.x="SHAREID",by.y="SHAREID",all.x=T) 
cohort <- merge(cohort,all_bmd,by.x="SHAREID",by.y="SHAREID",all.x=T)
cohort <- cohort %>%
  select(-IDTYPE.x, -IDTYPE.y)
cohort <- merge(cohort,OPIOID_drug,by.x="SHAREID",by.y="SHAREID",all.x=T)

cohort <- cohort %>%
  select(-DBGAP_SUBJECT_ID.x, -DBGAP_SUBJECT_ID.y)


cohort <- cohort %>%
  select(-ATT2, -ATT8, -DATE2, -DATE8, -AGE2, -AGE8) %>%
  mutate(OPIOID_USERS = coalesce(ifelse(PHRM_GP1 == "OPIOIDS", 1, 0), 0))

colnames(cohort)[colnames(cohort) == "SEX.x"] <- "Sex"
colnames(cohort)[colnames(cohort) == "AGE.x"] <- "Age"
colnames(cohort)[colnames(cohort)== "VALUE"] <- "BMD"

cohort$Sex <- factor(cohort$Sex, 
                     levels=c(1, 2),
                     labels=c("Male", "Female"))
colnames(cohort)
#miRNA associated with opioid use
library(tidyverse)
library(dplyr)
library(VennDiagram)
library(BiocManager)

library(ggplot2)
library(pheatmap)

library(haven)


# Specify the columns to move to the beginning
columns_to_move <- 418:441

# Reorder the columns and concatenate them at the beginning
cohort <- cohort[, c(columns_to_move, setdiff(1:ncol(cohort), columns_to_move))]




Colsums2 <- data.frame(variables = names(cohort), NAs = colSums(is.na(cohort))/nrow(cohort))
Colsums2$NAs[1:27]<- NA 
# Setup model specific data ####
# Model 1 Only (linear)
#expressed in 10% of samples
Colsums1 <- Colsums2[Colsums2$NAs < 0.9| is.na(Colsums2$NAs) == T,]
miRNA_pheno1 <- cohort[names(cohort) %in% Colsums1$variables] # Filter out miRNA with more than 10% NAs
names1 <- names(miRNA_pheno1)[-c(1:27)]

# Model 2 only (log/binary)
#expressed in less than 10% but more than 5% of sample

colsum<- Colsums2[(Colsums2$NAs > 0.9) & (Colsums2$NAs < 0.95) | is.na(Colsums2$NAs) == T,]
miRNA_pheno2 <- cohort[names(cohort) %in% colsum$variables]
miRNAdatna <- miRNA_pheno2[-c(1:27)]

miRNAdatna[!is.na(miRNAdatna)] <- 0 # Detectable
miRNAdatna[is.na(miRNAdatna)] <- 1 # Not Detectable
miRNAdatna[sapply(miRNAdatna, is.numeric)] <- lapply(miRNAdatna[sapply(miRNAdatna, is.numeric)], as.logical)
miRNA_pheno2 <- cbind(miRNA_pheno2[c(1:27)], miRNAdatna)
names2 <- names(miRNA_pheno2)[-c(1:27)]

mlr<- function(names, cov, data, var){
  name <- NULL
  mlr1 <- NULL
  for (i in 1:length(names)){
    #tryCatch({
    print(i)
    x <- lm(as.formula(paste(names[i], cov, sep = "")), data = data)
    y <- summary(x)
    z<- as.data.frame(y$coefficients)
    z$variable <- row.names(z)
    row.names(z)<- NULL
    if (i == 1){
      mlr1 <- z
      name <- rep(names[i], nrow(z))
    }else{
      mlr1 <- rbind(mlr1, z)
      name <- c(name, rep(names[i], nrow(z)))
    }
    #}, error = function(e){})
  }
  mlr_results <- cbind(miRNA = name, mlr1)
  mlr_fdr <- mlr_results %>% filter(., variable == var)
  mlr_fdr$FDR <- p.adjust(p = mlr_fdr$`Pr(>|t|)`, method = "BH", n = length(names)) 
  mlr_results <- merge(x = mlr_results, y = mlr_fdr, all.x = T)
  return(mlr_results)
}

mlrg <- function(names, cov, data, var){
  name <- NULL
  mlr1 <- NULL
  for (i in 1:length(names)){
    #tryCatch({
    print(i)
    x <- glm(as.formula(paste(names[i], cov, sep = "")), data = data, family = binomial(link="logit"))
    y <- summary(x)
    z <- as.data.frame(y$coefficients)
    z$variable <- row.names(z)
    row.names(z) <- NULL
    if (i == 1){
      mlr1 <- z
      name <- rep(names[i], nrow(z))
    } else {
      mlr1 <- rbind(mlr1, z)
      name <- c(name, rep(names[i], nrow(z)))
    }
  }
  mlr_results <- cbind(miRNA = name, mlr1)
  mlr_fdr <- mlr_results %>% filter(., variable == var)
  
  # Check if all p-values are zero
  if(all(mlr_results$`Pr(>|z|)` == 0)) {
    print("All p-values are zero. Skipping FDR adjustment.")
  } else {
    mlr_results$FDR <- p.adjust(p = mlr_results$`Pr(>|z|)`, method = "BH", n = nrow(mlr_results)) 
  }
  
  return(mlr_results)
}
BMDcov <- "~BMD+Age + Sex + BMI"
OPIOIDcov <- "~OPIOID_USERS + Age + Sex + BMI"


BMDresults1 <- mlr(names = names1, cov = BMDcov, data = miRNA_pheno1, var = "BMD")
OPIOIDresults1 <- mlr(names = names1, cov = OPIOIDcov, data = miRNA_pheno1, var = "OPIOID_USERS")

BMDresults2 <- mlrg(names = names2, cov = BMDcov, data = miRNA_pheno2, var = "BMD")
OPIOIDresults2 <- mlrg(names = names2, cov = OPIOIDcov, data = miRNA_pheno2, var = "OPIOID_USERS")



BMDresults1$model <- "linear"
BMDresults2$model <- "logistic"
OPIOIDresults1$model <- "linear"
OPIOIDresults2$model <- "logistic"
BMDresults1$Presence <- "90%"
BMDresults2$Presence <- "5-10%"
OPIOIDresults1$Presence <- "90%"
OPIOIDresults2$Presence <- "5-10%"
BMDresults1_p <- BMDresults1 %>%  filter(., variable == "BMD") %>% filter(., `Pr(>|t|)` < 0.05)

OPIOIDresults1_p <- OPIOIDresults1 %>%  filter(., variable == "OPIOID_USERS") %>% filter(., `Pr(>|t|)` < 0.05)

BMDresults2_p <- BMDresults2 %>%  filter(., variable == "BMD") %>% filter(., `Pr(>|z|)` < 0.05)

OPIOIDresults2_p <- OPIOIDresults2 %>%  filter(., variable == "OPIOID_USERS") %>% filter(., `Pr(>|z|)` < 0.05)


colnames(BMDresults1_p)[4] <- "t/z value"
colnames(BMDresults2_p)[4] <- "t/z value"
colnames(BMDresults1_p)[5] <- "P value"
colnames(BMDresults2_p)[5] <- "P value"

colnames(OPIOIDresults1_p)[4] <- "t/z value"
colnames(OPIOIDresults2_p)[4] <- "t/z value"
colnames(OPIOIDresults1_p)[5] <- "P value"
colnames(OPIOIDresults2_p)[5] <- "P value"

BMDMirna<-rbind(BMDresults1_p,BMDresults2_p)
BMDMirna <- BMDMirna[order(BMDMirna$`P value`), ]
OPIOIDMirna<-rbind(OPIOIDresults1_p,OPIOIDresults2_p)
OPIOIDMirna <- OPIOIDMirna[order(OPIOIDMirna$`P value`), ]

BMDMirna$Exponentiated_Estimate <- ifelse(BMDMirna$model == "logistic", exp(BMDMirna$Estimate), BMDMirna$Estimate)
OPIOIDMirna$Exponentiated_Estimate <- ifelse(OPIOIDMirna$model == "logistic", exp(OPIOIDMirna$Estimate), OPIOIDMirna$Estimate)

write.csv(BMDMirna, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/BMD_P_0.05.csv")
write.csv(OPIOIDMirna, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/OPIOID_P_0.05.csv")

#FOR VOLCANO PLOT

BMDresults1_p1 <- BMDresults1 %>%  filter(., variable == "BMD") 

OPIOIDresults1_p1 <- OPIOIDresults1 %>%  filter(., variable == "OPIOID_USERS") 

BMDresults2_p1 <- BMDresults2 %>%  filter(., variable == "BMD")

OPIOIDresults2_p1 <- OPIOIDresults2 %>%  filter(., variable == "OPIOID_USERS") 


colnames(BMDresults1_p1)[4] <- "t/z value"
colnames(BMDresults2_p1)[4] <- "t/z value"
colnames(BMDresults1_p1)[5] <- "P value"
colnames(BMDresults2_p1)[5] <- "P value"

colnames(OPIOIDresults1_p1)[4] <- "t/z value"
colnames(OPIOIDresults2_p1)[4] <- "t/z value"
colnames(OPIOIDresults1_p1)[5] <- "P value"
colnames(OPIOIDresults2_p1)[5] <- "P value"

VOLCANOBMDMirna<-rbind(BMDresults1_p1,BMDresults2_p1)
VOLCANOBMDMirna <- VOLCANOBMDMirna[order(VOLCANOBMDMirna$`P value`), ]
VOLCANOOPIOIDMirna<-rbind(OPIOIDresults1_p1,OPIOIDresults2_p1)
VOLCANOOPIOIDMirna <- VOLCANOOPIOIDMirna[order(VOLCANOOPIOIDMirna$`P value`), ]

VOLCANOBMDMirna$Exponentiated_Estimate <- ifelse(VOLCANOBMDMirna$model == "logistic", exp(VOLCANOBMDMirna$Estimate), VOLCANOBMDMirna$Estimate)
VOLCANOOPIOIDMirna$Exponentiated_Estimate <- ifelse(VOLCANOOPIOIDMirna$model == "logistic", exp(VOLCANOOPIOIDMirna$Estimate), VOLCANOOPIOIDMirna$Estimate)

write.csv(VOLCANOBMDMirna, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/VOLCANOBMDMirna.csv")
write.csv(VOLCANOOPIOIDMirna, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/VOLCANOOPIOIDMirna.csv")


#intersection
BMDall <- c(as.character(BMDMirna$miRNA))
OPIOIDall <- c(as.character(OPIOIDMirna$miRNA))

int<- intersect(BMDall, OPIOIDall)
list(int)

#Now do the interaction

matching_name1 <- names1[names1 %in% int]

#model interacted snps

mlr <- function(names, cov, data){
  name <- NULL
  mlr1 <- NULL
  for (i in 1:length(names)){
    print(i)
    x <- lm(as.formula(paste(names[i], cov, sep = "")), data = data)
    y <- summary(x)
    z <- as.data.frame(y$coefficients)
    z$variable <- row.names(z)
    row.names(z) <- NULL
    if (i == 1){
      mlr1 <- z
      name <- rep(names[i], nrow(z))
    } else {
      mlr1 <- rbind(mlr1, z)
      name <- c(name, rep(names[i], nrow(z)))
    }
  }
  mlr_results <- cbind(miRNA = name, mlr1)
  mlr_results$FDR <- p.adjust(p = mlr_results$`Pr(>|t|)`, method = "BH", n = nrow(mlr_results)) 
  return(mlr_results)
}

bcov <- "~BMD+OPIOID_USERS+Age + Sex + BMI"
bcov2 <- "~BMD*OPIOID_USERS+Age + Sex + BMI"


results1 <- mlr(names = matching_name1, cov = bcov, data = miRNA_pheno1)
results2 <- mlr(names = matching_name1, cov = bcov2, data = miRNA_pheno1)

results1_p <- results1 %>%
  filter(variable %in% c("BMD", "OPIOID_USERS"))

results2_p <- results2 %>%
  filter(variable %in% c("BMD", "OPIOID_USERS","BMD:OPIOID_USERS"))

write.csv(results1_p, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/INT_MIRNA~BMD+OPIOID.csv")
write.csv(results2_p, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/INT_MIRNA~BMD*OPIOID.csv")

#bmd as y variable


#Katie's model


mlr<- function(var, names, cov, data){
  name <- NULL
  mlr1 <- NULL
  for (i in 1:length(names)){
    #tryCatch({
    print(i)
    x <- lm(as.formula(paste(var, paste(names[i], cov, sep = " + "), sep = " ~ ")), data = data)
    y <- summary(x)
    z<- as.data.frame(y$coefficients)
    z$variable <- row.names(z)
    row.names(z)<- NULL
    if (i == 1){
      mlr1 <- z
      name <- rep(names[i], nrow(z))
    }else{
      mlr1 <- rbind(mlr1, z)
      name <- c(name, rep(names[i], nrow(z)))
    }
    #}, error = function(e){})
  }
  mlr_results <- cbind(miRNA = name, mlr1)
  mlr_fdr <- mlr_results %>% filter(., miRNA == var)
  mlr_fdr$FDR <- p.adjust(p = mlr_fdr$`Pr(>|t|)`, method = "BH", n = length(names))
  mlr_results <- merge(x = mlr_results, y = mlr_fdr, all.x = T)
  return(mlr_results)
}
BMDcov <- "Age + Sex + BMI"


BMDresults1 <- mlr( var = "BMD",names = names1, cov = BMDcov, data = miRNA_pheno1)
BMDresults1$FDR <- p.adjust(p = BMDresults1$`Pr(>|t|)`, method = "BH", n = nrow(BMDresults1))

BMDresults1_p <- BMDresults1 %>% filter(., miRNA == variable) %>% filter(., `Pr(>|t|)` < 0.05)




# Model 2 only (log/binary)

BMDresults2 <- mlr( var = "BMD",names = as.factor(names2), cov = BMDcov, data = cohort)

BMDresults2$FDR <- p.adjust(p = BMDresults2$`Pr(>|t|)`, method = "BH", n = nrow(BMDresults2))


BMDresults2_p <- BMDresults2 %>% filter(., miRNA == variable) %>% filter(., `Pr(>|t|)` < 0.05)




BMDMirna<-rbind(BMDresults1_p,BMDresults2_p)
write.csv(BMDMirna, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/Y-BMD-P-0.05.csv")

#FOR VOLCANO

BMDresults1_p <- BMDresults1 %>% filter(., miRNA == variable) 
BMDresults2_p <- BMDresults2 %>% filter(., miRNA == variable) 

BMDMirna<-rbind(BMDresults1_p,BMDresults2_p)
write.csv(BMDMirna, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/VOLCANO-Y-BMD-P-0.05.csv")


#intersection
BMDall <- c(as.character(BMDMirna$miRNA))
OPIOIDall <- c(as.character(OPIOIDMirna$miRNA))

int<- intersect(BMDall, OPIOIDall)
list(int)

#Now do the interaction

matching_name1 <- names1[names1 %in% int]


# Define a function to create linear regression models
create_models <- function(variable_name, data) {
  formula <- as.formula(paste("BMD ~", variable_name, "* OPIOID_USERS + Age + Sex + BMI"))
  model <- lm(formula, data = data)
  return(model)
}

# Apply the function to each variable in matching_name1
models_list <- lapply(matching_name1, create_models, data = miRNA_pheno1)

# Access the summary of each model
summaries <- lapply(models_list, summary)


#table
# Initialize an empty data frame to store coefficients
coefficients_table <- data.frame()

# Loop through each model summary
for (i in seq_along(summaries)) {
  # Extract coefficients
  coefficients <- summaries[[i]]$coefficients
  
  # Create a data frame for the coefficients
  model_coefficients <- data.frame(
    variable = rownames(coefficients),
    coefficient = coefficients[, 1],
    std.error = coefficients[, 2],
    t.value = coefficients[, 3],
    p.value = coefficients[, 4],
    model = i
  )
  
  # Append coefficients to the coefficients_table
  coefficients_table <- rbind(coefficients_table, model_coefficients)
}

# Print the coefficients table
print(coefficients_table)
write.csv(coefficients_table, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/int_bmd*mirnaopioid+cov.csv", row.names = FALSE)

getwd()

#int-next model
create_models <- function(variable_name, data) {
  formula <- as.formula(paste("BMD ~", paste(variable_name, "+ OPIOID_USERS", collapse = " + "), "+ Age + Sex + BMI"))
  model <- lm(formula, data = data)
  return(model)
}

# Apply the function to each variable in matching_name1
models_list <- lapply(matching_name1, create_models, data = miRNA_pheno1)

# Access the summary of each model
summaries <- lapply(models_list, summary)


#table
# Initialize an empty data frame to store coefficients
coefficients_table <- data.frame()

# Loop through each model summary
for (i in seq_along(summaries)) {
  # Extract coefficients
  coefficients <- summaries[[i]]$coefficients
  
  # Create a data frame for the coefficients
  model_coefficients <- data.frame(
    variable = rownames(coefficients),
    coefficient = coefficients[, 1],
    std.error = coefficients[, 2],
    t.value = coefficients[, 3],
    p.value = coefficients[, 4],
    model = i
  )
  
  # Append coefficients to the coefficients_table
  coefficients_table <- rbind(coefficients_table, model_coefficients)
}

# Print the coefficients table
print(coefficients_table)
write.csv(coefficients_table, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/int_bmd~mirna+opioid+cov.csv", row.names = FALSE)



#FIGURES


library(Hmisc)
library(gplots)
#library(tidyr)
library(dplyr)
#library(tidyverse)
library(MASS)
library(lme4)
library(tidyr)
library(heatmaply)
library(patchwork)
library(cowplot)
library(ggplot2)
library(ggrepel)
library(calibrate)

setwd("/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/")
#OPIOID VOLCANO 
df <- read.csv("VOLCANOOPIOIDMirna.csv", header = T)

df$miRNA <- gsub("_", "-", tolower(df$miRNA))
df$miRNA <- gsub("mir", "miR", df$miRNA)

df$diffexpressed<-"NO"


df$diffexpressed[df$Estimate >= 0.5 & df$P.value < 0.05] <- "UP"
df$diffexpressed[df$Estimate <= -0.5 & df$P.value < 0.05] <- "DOWN"


# Get the top 10 miRNAs with the lowest p-values
top_8_miRNAs <- head(df[order(df$P.value), "miRNA"], 10)

# Create the delabel column based on conditions
df$delabel <- ifelse(df$miRNA %in% top_8_miRNAs, df$miRNA, NA)




library(ggrepel)

Endpoint2 <- ggplot(data = df, aes(x = Estimate, y = -log10(P.value), col = diffexpressed, label = delabel)) +
  geom_vline(xintercept = c(-0.5, 0.5), col = "grey", linetype = 'dashed') +
  geom_text_repel(data = df, aes(label = delabel), 
                  point.size = NA, size = 9, max.overlaps = Inf, colour = "black",  
                  segment.ncp = 3, segment.angle = 50, hjust = -0.18) +  # Adjusted parameters
  theme_bw(base_size = 24) +  # Increased base font size
  geom_hline(yintercept = -log10(0.05), col = "grey", linetype = 'dashed') +
  geom_point(size = 2) +  # Increased point size
  scale_color_manual(values = c("blue", "black", "red"),
                     labels = c("Downregulated", "Not significant", "Upregulated"),
                     name = NULL) +  # Set legend title to NULL
  theme_set(theme_classic(base_size = 22)) +  # Increased base font size
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 2.8, face = "bold"),  # Increased size of legend title
    axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0), size = rel(1.6), color = 'black', family = "Helvetica"),  # Increased size
    axis.title.x = element_text(hjust = 0.5, face = "bold", margin = margin(20, 0, 0, 0), size = rel(1.6), color = 'black', family = "Helvetica"),  # Increased size
    plot.title = element_text(hjust = 0.5, family = "Helvetica", size = 28, face = "bold"),  # Increased size of plot title
    text = element_text(family = "Helvetica", size = 24)  # Increased size
  ) +
  labs(x = expression("log"[2] * "(Fold Change)"), y = expression("-log"[10] * "(p-value)")) +
  ggtitle('miRNAs associated with Opioid use')
saveRDS(Endpoint2, file = "opioid_microRNA")
ggsave("NEWopioid_microRNA.png", plot = Endpoint2, width = 10, height = 7, dpi = 400)


#BMD VOLCANO 
df <- read.csv("VOLCANOBMDMirna.csv", header = T)

df$miRNA <- gsub("_", "-", tolower(df$miRNA))
df$miRNA <- gsub("mir", "miR", df$miRNA)

df$diffexpressed<-"NO"


df$diffexpressed[df$Estimate >= 0.5 & df$P.value < 0.05] <- "UP"
df$diffexpressed[df$Estimate <= -0.5 & df$P.value < 0.05] <- "DOWN"


# Get the top 10 miRNAs with the lowest p-values
top_8_miRNAs <- head(df[order(df$P.value), "miRNA"], 10)

# Create the delabel column based on conditions
df$delabel <- ifelse(df$miRNA %in% top_8_miRNAs, df$miRNA, NA)




library(ggrepel)

Endpoint2 <- ggplot(data = df, aes(x = Estimate, y = -log10(P.value), col = diffexpressed, label = delabel)) +
  geom_vline(xintercept = c(-0.5, 0.5), col = "grey", linetype = 'dashed') +
  geom_text_repel(data = df, aes(label = delabel), 
                  point.size = NA, size = 9, max.overlaps = Inf, colour = "black",  
                  segment.ncp = 3, segment.angle = 50, hjust = -0.18) +  # Adjusted parameters
  theme_bw(base_size = 24) +  # Increased base font size
  geom_hline(yintercept = -log10(0.05), col = "grey", linetype = 'dashed') +
  geom_point(size = 2) +  # Increased point size
  scale_color_manual(values = c("blue", "black", "red"),
                     labels = c("Downregulated", "Not significant", "Upregulated"),
                     name = NULL) +  # Set legend title to NULL
  theme_set(theme_classic(base_size = 22)) +  # Increased base font size
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 2.8, face = "bold"),  # Increased size of legend title
    axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0), size = rel(1.6), color = 'black', family = "Helvetica"),  # Increased size
    axis.title.x = element_text(hjust = 0.5, face = "bold", margin = margin(20, 0, 0, 0), size = rel(1.6), color = 'black', family = "Helvetica"),  # Increased size
    plot.title = element_text(hjust = 0.5, family = "Helvetica", size = 28, face = "bold"),  # Increased size of plot title
    text = element_text(family = "Helvetica", size = 24)  # Increased size
  ) +
  labs(x = expression("log"[2] * "(Fold Change)"), y = expression("-log"[10] * "(p-value)")) +
  ggtitle('miRNAs associated with BMD')
saveRDS(Endpoint2, file = "bmd_microRNA")
ggsave("NEWbmd_microRNA.png", plot = Endpoint2, width = 10, height = 7, dpi = 400)


#BMD VOLCANO-2-y
df <- read.csv("VOLCANO-Y-BMD-P-0.05.csv", header = T)

df$miRNA <- gsub("_", "-", tolower(df$miRNA))
df$miRNA <- gsub("mir", "miR", df$miRNA)
df$Estimate <- df$Estimate * 1000

df$diffexpressed<-"NO"


df$diffexpressed[df$Estimate >= 0.5 & df$Pr...t.. < 0.05] <- "UP"
df$diffexpressed[df$Estimate <= -0.5 & df$Pr...t.. < 0.05] <- "DOWN"


# Get the top 10 miRNAs with the lowest p-values
top_8_miRNAs <- head(df[order(df$Pr...t..), "miRNA"], 8)

# Create the delabel column based on conditions
df$delabel <- ifelse(df$miRNA %in% top_8_miRNAs, df$miRNA, NA)



library(ggrepel)

Endpoint2 <- ggplot(data = df, aes(x = Estimate, y = -log10(Pr...t..), col = diffexpressed, label = delabel)) +
  geom_vline(xintercept = c(-1, 1), col = "grey", linetype = 'dashed') +
  geom_text_repel(data = df, aes(label = delabel), 
                  point.size = NA, size = 9, max.overlaps = Inf, colour = "black",  
                  segment.ncp = 3, segment.angle = 50, hjust = -0.18) +  # Adjusted parameters
  theme_bw(base_size = 24) +  # Increased base font size
  geom_hline(yintercept = -log10(0.05), col = "grey", linetype = 'dashed') +
  geom_point(size = 2) +  # Increased point size
  scale_color_manual(values = c("blue", "black", "red"),
                     labels = c("Downregulated", "Not significant", "Upregulated"),
                     name = NULL) +  # Set legend title to NULL
  theme_set(theme_classic(base_size = 22)) +  # Increased base font size
  theme(
    legend.position = "top", 
    legend.title = element_text(size = 2.8, face = "bold"),  # Increased size of legend title
    axis.title.y = element_text(face = "bold", margin = margin(0, 20, 0, 0), size = rel(1.6), color = 'black', family = "Helvetica"),  # Increased size
    axis.title.x = element_text(hjust = 0.5, face = "bold", margin = margin(20, 0, 0, 0), size = rel(1.6), color = 'black', family = "Helvetica"),  # Increased size
    plot.title = element_text(hjust = 0.5, family = "Helvetica", size = 28, face = "bold"),  # Increased size of plot title
    text = element_text(family = "Helvetica", size = 24)  # Increased size
  ) +
  labs(x = expression("log"[2] * "(Fold Change)"), y = expression("-log"[10] * "(p-value)")) +
  ggtitle('miRNAs associated with BMD')
saveRDS(Endpoint2, file = "bmd_microRNA")
ggsave("Y-BMDNEWbmd_microRNA.png", plot = Endpoint2, width = 10, height = 7, dpi = 400)


#ven diagram

library(VennDiagram)
grid.newpage()
grid.draw(venn.plot)

# Create the Venn diagram
venn_plot <- draw.pairwise.venn(
  area1 = 64, area2 = 28, cross.area = 10,
  category = c("BMD miRNA", "Opioid miRNA"),
  border = "black",
  lty = rep("blank", 2),
  fill = c(alpha("#440154ff", 0.3), alpha("#21908dff", 0.3)),
  alpha = rep(0.5, 2),
  cat.pos = c(0, 15),  # Adjust the position of the "Opioid miRNA" category label to the left
  cat.dist = rep(0.025, 2.5),
  cat.cex = 2,
  cex.num = 6,
  cat.col = c("#440154ff", '#21908dff')
)

png(file = "NEWvenn_diagram.png", width = 800, height = 600, units = "px", pointsize = 20, bg = "white")
grid.draw(venn_plot)
dev.off()

venn_plot <- draw.pairwise.venn(
  area1 = 65, area2 = 28, cross.area = 10,
  category = c("BMD miRNA", "Opioid miRNA"),
  border = "black",
  lty = rep("blank", 2),
  fill = c(alpha("#440154ff", 0.3), alpha("#21908dff", 0.3)),
  alpha = rep(0.5, 2),
  cat.pos = c(0, 15),  # Adjust the position of the "Opioid miRNA" category label to the left
  cat.dist = rep(0.025, 2.5),
  cat.cex = 2,
  cex.num = 6,
  cat.col = c("#440154ff", '#21908dff')
)

png(file = "YNEWvenn_diagram.png", width = 800, height = 600, units = "px", pointsize = 20, bg = "white")
grid.draw(venn_plot)
dev.off()

#Forest plot
int
df<-read.csv("BMD_P_0.05.csv", header=T)
df2<-read.csv("OPIOID_P_0.05.csv", header=T)
int_miRNAs1 <- subset(df, miRNA %in% int)
int_miRNAs2 <- subset(df2, miRNA %in% int)
df3<-read.csv("Y-BMD-P-0.05.csv", header=T)
int_miRNAs3 <- subset(df3, miRNA %in% int)
merged_int_miRNAs <- merge(int_miRNAs1, int_miRNAs2, by = "miRNA")
merged_int_miRNAs1 <- merged_int_miRNAs1[, c("miRNA", "Estimate.x", "Estimate.y","Std..Error.x","Std..Error.y")]
write.csv(merged_int_miRNAs1, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/Forest.csv", row.names = FALSE)

merged_int_miRNAs2 <- merge(int_miRNAs2, int_miRNAs3, by = "miRNA")
merged_int_miRNAs3 <- merged_int_miRNAs2[, c("miRNA", "Estimate.x", "Estimate.y","Std..Error.x","Std..Error.y")]
write.csv(merged_int_miRNAs3, "/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/YForest.csv", row.names = FALSE)



library(ggplot2)

# Example data (replace this with your actual data)
forest_data <- read.csv("Forest.csv", header = T)

extracted_data <- forest_data[, 1:3]

# Displaying the extracted data
print(extracted_data)

extracted_data$miRNA <- gsub("_", "-", tolower(extracted_data$miRNA))
extracted_data$miRNA <- gsub("mir", "miR", extracted_data$miRNA)



# Melt the data for ggplot2
library(reshape2)
melted_data <- melt(extracted_data, id.vars = "miRNA")



grouped_bar_plot <- ggplot(melted_data, aes(x = miRNA, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(x = "miRNA", y = "Effect Size", fill = "") +  # Remove legend title
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 20, vjust = 0.5, face = "bold"),  # Increase size and make bold
    axis.title.x = element_text(size = 20, face = "bold"),  # Increase size and make bold
    axis.text = element_text(size = 17,face = "bold"),  # Adjust axis text size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "top",  # Move legend to bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 17),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5, size = 22,face = "bold")  # Center the plot title
  ) +
  scale_fill_manual(
    name = "",  # Change legend title to empty string
    labels = c("BMD", "Opioid Use"),  # Change legend labels
    values = c("Estimate.x" = "#4daf4a", "Estimate.y" = "#377eb8")
  ) +
  ggtitle("miRNA Effect: Opioid Use vs BMD")


# Save the forest plot as an RDS file
saveRDS(grouped_bar_plot, file = "forest_plot.rds")

# Save the forest plot as a PNG with high resolution
ggsave("forest_plot.png", plot = grouped_bar_plot, width = 10, height = 7, dpi = 300)


#FORST PLOT Y 

forest_data <- read.csv("YForest.csv", header = T)
forest_data$miRNA <- gsub("_", "-", tolower(forest_data$miRNA))
forest_data$miRNA <- gsub("mir", "miR", forest_data$miRNA)

extracted_data <- forest_data[, 1:3]
extracted_data$Estimate.y<- extracted_data$Estimate.y * 1000

melted_data <- melt(extracted_data, id.vars = "miRNA")



grouped_bar_plot <- ggplot(melted_data, aes(x = miRNA, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  coord_flip() +
  labs(x = "miRNA", y = "Effect Size", fill = "") +  # Remove legend title
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 20, vjust = 0.5, face = "bold"),  # Increase size and make bold
    axis.title.x = element_text(size = 20, face = "bold"),  # Increase size and make bold
    axis.text = element_text(size = 17,face = "bold"),  # Adjust axis text size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "top",  # Move legend to bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 17),  # Adjust legend text size
    plot.title = element_text(hjust = 0.5, size = 22,face = "bold")  # Center the plot title
  ) +
  scale_fill_manual(
    name = "",  # Change legend title to empty string
    labels = c("Opioid use", "BMD"),  # Change legend labels
    values = c("Estimate.x" = "#4daf4a", "Estimate.y" = "#377eb8")
  ) +
  ggtitle("miRNA Effect: Opioid Use vs BMD")




# Save the forest plot as an RDS file
saveRDS(grouped_bar_plot, file = "forest_plot2.rds")

# Save the forest plot as a PNG with high resolution
ggsave("forest_plot2.png", plot = grouped_bar_plot, width = 10, height = 7, dpi = 300)


#forest plot another style
install.packages("forestplot")
library(forestplot)



forest_data <- read.csv("Forest.csv", header = T)
forest_data$miRNA <- gsub("_", "-", tolower(forest_data$miRNA))
forest_data$miRNA <- gsub("mir", "miR", forest_data$miRNA)

colnames(forest_data) <- c("miRNA", "Estimate_BMD", "Estimate_Opioid", "Std_Error_BMD", "Std_Error_Opioid")

# Calculate confidence intervals
forest_data <- forest_data %>%
  mutate(lower_Opioid = Estimate_Opioid - 1.96 * Std_Error_Opioid,
         upper_Opioid = Estimate_Opioid + 1.96 * Std_Error_Opioid,
         lower_BMD = Estimate_BMD - 1.96 * Std_Error_BMD,
         upper_BMD = Estimate_BMD + 1.96 * Std_Error_BMD)

new_forest_data <- data.frame(
  miRNA = forest_data$miRNA,
  Estimate = c(forest_data$Estimate_BMD, forest_data$Estimate_Opioid),
  Lower = c(forest_data$lower_BMD, forest_data$lower_Opioid),
  Upper = c(forest_data$upper_BMD, forest_data$upper_Opioid),
  Group = rep(c("BMD", "Opioid"), each = nrow(forest_data))
)






# Create the forest plot
forest_plot <- ggplot(new_forest_data, aes(x = Estimate, y = miRNA)) +
  geom_point(aes(color = Group), size = 7, shape = 15) + # Make points square
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0) + # Adjust height for square error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + # Add dashed line at 0
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "white"), # White background for legend
    panel.background = element_rect(fill = "white"), # White background
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line.y = element_line(color = "black"), # Color of y-axis lines
    axis.ticks = element_line(color = "black"), # Color of ticks
    axis.title = element_text(size = 16, face = "bold"), # Bold font size of axis titles
    axis.text = element_text(size = 14, face = "bold"), # Bold font size of axis text
    legend.text = element_text(size = 14, face = "bold"), # Bold font size of legend text
    legend.title = element_text(size = 16, face = "bold") # Bold font size of legend title
  ) +
  labs(x = "Estimate", y = "miRNA", color = "Group") +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.5)) # Adjust x-axis scale

# Show the plot
print(forest_plot)
saveRDS(grouped_bar_plot, file = "forest_plot2.rds")

# Save the forest plot as a PNG with high resolution
ggsave("newforest_plot1.png", plot = forest_plot, width = 10, height = 7, dpi = 300)

#another one 


forest_data <- read.csv("YForest.csv", header = T)
forest_data$miRNA <- gsub("_", "-", tolower(forest_data$miRNA))
forest_data$miRNA <- gsub("mir", "miR", forest_data$miRNA)
forest_data$Estimate.y<- forest_data$Estimate.y * 1000
forest_data$Std..Error.y<- forest_data$Std..Error.y * 1000

colnames(forest_data) <- c("miRNA", "Estimate_BMD", "Estimate_Opioid", "Std_Error_BMD", "Std_Error_Opioid")

# Calculate confidence intervals
forest_data <- forest_data %>%
  mutate(lower_Opioid = Estimate_Opioid - 1.96 * Std_Error_Opioid,
         upper_Opioid = Estimate_Opioid + 1.96 * Std_Error_Opioid,
         lower_BMD = Estimate_BMD - 1.96 * Std_Error_BMD,
         upper_BMD = Estimate_BMD + 1.96 * Std_Error_BMD)

new_forest_data <- data.frame(
  miRNA = forest_data$miRNA,
  Estimate = c(forest_data$Estimate_BMD, forest_data$Estimate_Opioid),
  Lower = c(forest_data$lower_BMD, forest_data$lower_Opioid),
  Upper = c(forest_data$upper_BMD, forest_data$upper_Opioid),
  Group = rep(c("BMD", "Opioid"), each = nrow(forest_data))
)






# Create the forest plot
forest_plot <- ggplot(new_forest_data, aes(x = Estimate, y = miRNA)) +
  geom_point(aes(color = Group), size = 7, shape = 15) + # Make points square
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0) + # Adjust height for square error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + # Add dashed line at 0
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.background = element_rect(fill = "white"), # White background for legend
    panel.background = element_rect(fill = "white"), # White background
    panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    axis.line.y = element_line(color = "black"), # Color of y-axis lines
    axis.ticks = element_line(color = "black"), # Color of ticks
    axis.title = element_text(size = 16, face = "bold"), # Bold font size of axis titles
    axis.text = element_text(size = 14, face = "bold"), # Bold font size of axis text
    legend.text = element_text(size = 14, face = "bold"), # Bold font size of legend text
    legend.title = element_text(size = 16, face = "bold") # Bold font size of legend title
  ) +
  labs(x = "Estimate", y = "miRNA", color = "Group") +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.5)) # Adjust x-axis scale

# Show the plot
print(forest_plot)
saveRDS(forest_plot, file = "YNEWforest_plot2.rds")

# Save the forest plot as a PNG with high resolution
ggsave("newforest_plotY.png", plot = forest_plot, width = 10, height = 7, dpi = 300)


library(emmeans)
library(ggplot2)

model <- lm(BMD ~ OPIOID_USERS+Age+Sex + BMI, data = cohort)
summary(model)
# Obtain emmeans
emmeans_values <- emmeans(model, "OPIOID_USERS")

# Convert emmeans values to a data frame
emmeans_df <- as.data.frame(emmeans_values)
p <- ggplot(data = emmeans_df, aes(x = factor(OPIOID_USERS), y = emmean)) +
  geom_bar(stat = "identity") +
  theme_minimal()

p_value <- summary(model)$coefficients["OPIOID_USERS", "Pr(>|t|)"]

emmeans_plot_centered_rds<-ggplot(emmeans_df, aes(x = factor(OPIOID_USERS, labels = c("Non Users", "Users")), y = emmean)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), fill = "blue", width = 0.3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = 0.5), width = 0.2) +
  labs(x = "Opioid Users", y = "Mean BMD", title = "Mean BMD between Non Users and Users") +
  geom_text(aes(x = 1.5, y = max(emmeans_df$emmean) + 0.2, label = paste("p =", format(p_value, scientific = TRUE, digits = 2))),
            hjust = 0.5, size = 6) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", margin = margin(0, 0, 20, 0),hjust = 0.5),
    plot.caption = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10, face = "bold", margin = margin(0, 0, 20, 0)),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    legend.position = "none"
  )
violin_plot <- ggplot(emmeans_df, aes(x = factor(OPIOID_USERS), y = emmean)) +
  geom_violin(fill = "skyblue", color = "black") + # Create violin plot
  geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) + # Add boxplot for median and quartiles
  labs(x = "Opioid Users", y = "Estimated Mean", title = "Violin Plot of Estimated Means") +
  theme_minimal()

#TABLES
library(dplyr)
library(flextable)

getwd()
setwd("/work/larylab/NAYEMA/MIRNA_OPIOID_BMD_ALL_FINAL_SCRIPTS/SCRIPTS/final full script/result_more_than_10/")
table1<-read.csv("OPIOID_P_0.05.csv",header=T)
colnames(table1)
table1$miRNA <- gsub("_", "-", tolower(table1$miRNA))
table1$miRNA <- gsub("mir", "miR", table1$miRNA)


plotting_columns <- table1 %>% 
  mutate(P.value = round(P.value, digits = 3),
         Estimate = round(Estimate, digits = 3),
         SE = round(`Std..Error`, digits = 3),
         FDR = round(FDR, digits = 3)) %>% 
  select(miRNA, Estimate, SE, P.value, variable, FDR)

set_flextable_defaults(
  font.size = 8, theme_fun = theme_vanilla,
  padding = 6,
  tabcolsep = 0,
  line_spacing = .8,
  text.align = 'center',
  #background.color = "#EFEFEF"
)
flextable <- flextable(plotting_columns) %>% 
  set_table_properties(align = 'right', layout = 'autofit')


flextable
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/mirna~opioid-table1.docx")

#TABLE2
table2<-read.csv("BMD_P_0.05.csv",header=T)
colnames(table2)
table2$miRNA <- gsub("_", "-", tolower(table2$miRNA))
table2$miRNA <- gsub("mir", "miR", table2$miRNA)


plotting_columns <- table2 %>% 
  mutate(P.value = round(P.value, digits = 3),
         Estimate = round(Estimate, digits = 3),
         SE = round(`Std..Error`, digits = 3),
         FDR = round(FDR, digits = 3)) %>% 
  select(miRNA, Estimate, SE, P.value, variable, FDR)

set_flextable_defaults(
  font.size = 8, theme_fun = theme_vanilla,
  padding = 6,
  tabcolsep = 0,
  line_spacing = .8,
  text.align = 'center',
  #background.color = "#EFEFEF"
)
flextable <- flextable(plotting_columns) %>% 
  set_table_properties(align = 'right', layout = 'autofit')


flextable
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/mirna~BMD-table2.docx")

#TABLE3

table3<-read.csv("Y-BMD-P-0.05.csv",header=T)

colnames(table3)
table3$miRNA <- gsub("_", "-", tolower(table3$miRNA))
table3$miRNA <- gsub("mir", "miR", table3$miRNA)


plotting_columns <- table3 %>% 
  mutate( P.value = round(Pr...t.., digits = 3),
         Estimate = round(Estimate, digits = 3),
         SE = round(`Std..Error`, digits = 3),
         FDR = round(FDR, digits = 3)) %>% 
  select(miRNA, Estimate, SE, P.value,FDR)

set_flextable_defaults(
  font.size = 8, theme_fun = theme_vanilla,
  padding = 6,
  tabcolsep = 0,
  line_spacing = .8,
  text.align = 'center',
  #background.color = "#EFEFEF"
)
flextable <- flextable(plotting_columns) %>% 
  set_table_properties(align = 'right', layout = 'autofit')


flextable
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/BMD~mirna-table3.docx")

# 

table4<-read.csv("int_bmd*mirnaopioid+cov.csv",header=T)

table4 <- table4[!table4$variable %in% c("Age", "SexFemale", "BMI","(Intercept)"), ]

plotting_columns <- table4 %>% 
  mutate(p.value = round(p.value, digits = 3),
          Estimate = round(coefficient, digits = 3),
          SE = round(std.error, digits = 3)) %>% 
  select(variable, Estimate, SE, p.value)
  


set_flextable_defaults(
  font.size = 8, theme_fun = theme_vanilla,
  padding = 6,
  tabcolsep = 0,
  line_spacing = .8,
  text.align = 'center',
  #background.color = "#EFEFEF"
)
flextable <- flextable(plotting_columns) %>% 
  set_table_properties(align = 'right', layout = 'autofit')

flextable
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/BMD~mirna*opioid-table4.docx")

#table 5

table5<-read.csv("int_bmd~mirna+opioid+cov.csv", header=T)

table5 <- table5[!table5$variable %in% c("Age", "SexFemale", "BMI","(Intercept)"), ]

plotting_columns <- table5 %>% 
  mutate(p.value = round(p.value, digits = 3),
         Estimate = round(coefficient, digits = 3),
         SE = round(std.error, digits = 3)) %>% 
  select(variable, Estimate, SE, p.value)



set_flextable_defaults(
  font.size = 8, theme_fun = theme_vanilla,
  padding = 6,
  tabcolsep = 0,
  line_spacing = .8,
  text.align = 'center',
  #background.color = "#EFEFEF"
)
flextable <- flextable(plotting_columns) %>% 
  set_table_properties(align = 'right', layout = 'autofit')

flextable
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/BMD~mirna+opioid-table5.docx")

#TABLE 6

table6<-read.csv("INT_MIRNA~BMD*OPIOID.csv", header=T)

plotting_columns <- table6 %>% 
  mutate(p.value = round(Pr...t.., digits = 3),
         Estimate = round(Estimate, digits = 3),
         FDR=round(FDR, digits = 3),
         SE = round(Std..Error, digits = 3)) %>% 
  select(miRNA,variable, Estimate, SE, p.value,FDR)

set_flextable_defaults(
  font.size = 8, theme_fun = theme_vanilla,
  padding = 6,
  tabcolsep = 0,
  line_spacing = .8,
  text.align = 'center',
  #background.color = "#EFEFEF"
)
flextable <- flextable(plotting_columns) %>% 
  set_table_properties(align = 'right', layout = 'autofit')

flextable
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/mirna~BMD*opioid-table6.docx")

#Table 7 

table7<-read.csv("INT_MIRNA~BMD+OPIOID.csv", header=T)

plotting_columns <- table7 %>% 
  mutate(p.value = round(Pr...t.., digits = 3),
         Estimate = round(Estimate, digits = 3),
         FDR=round(FDR, digits = 3),
         SE = round(Std..Error, digits = 3)) %>% 
  select(miRNA,variable, Estimate, SE, p.value,FDR)

set_flextable_defaults(
  font.size = 8, theme_fun = theme_vanilla,
  padding = 6,
  tabcolsep = 0,
  line_spacing = .8,
  text.align = 'center',
  #background.color = "#EFEFEF"
)
flextable <- flextable(plotting_columns) %>% 
  set_table_properties(align = 'right', layout = 'autofit')

flextable
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/mirna~BMD+opioid-table7.docx")

#emmeans

library(emmeans)
library(ggplot2)

# Assuming 'opioid_users' is the variable indicating opioid users (1 and 0)
# and 'value' is the variable you want to plot
model <- lm(BMD~ OPIOID_USERS + Age + Sex + BMI, data = cohort)

# Summary of the linear model
summary(model)

# Obtain emmeans
emmeans_values <- emmeans(model, "OPIOID_USERS")

# Convert emmeans values to a data frame
emmeans_df <- as.data.frame(emmeans_values)

# Extract the p-value for 'OPIOID_USERS'
p_value <- summary(model)$coefficients["OPIOID_USERS", "Pr(>|t|)"]




cohort$OPIOID_USERS <- factor(cohort$OPIOID_USERS, levels = c(0, 1), labels = c("Non-user", "User"))

# Create the box plot using ggplot2 with updated labels
p <- ggplot(cohort, aes(x = OPIOID_USERS, y = BMD, fill = OPIOID_USERS)) +
  geom_boxplot(width = 0.5, color = "black", alpha = 0.8) +
  labs(x = "Opioid Users", y = "BMD",
       title = paste("Box Plot of BMD by Opioid Users"),
       fill = "Opioid Users") +
  theme_minimal() +
  theme(legend.title = element_text(size = 16, face = "bold"),  # Set legend title size and make it bold
        legend.text = element_text(size = 16, face = "bold"),   # Set legend text size and make it bold
        axis.title.x = element_text(size = 20, face = "bold"),   # Set x-axis title size and make it bold
        axis.title.y = element_text(size = 20, face = "bold"),   # Set y-axis title size and make it bold
        axis.text.x = element_text(size = 16, face = "bold"),    # Set x-axis text size and make it bold
        axis.text.y = element_text(size = 16, face = "bold"),    # Set y-axis text size and make it bold
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),  # Set title size and center alignment and make it bold
        legend.position = "top",  # Position legend at the top
        panel.background = element_rect(fill = "white")) +  # Set background color to white
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), 
                    labels = c("Non-user", "User"))  # Set custom fill colors and labels

# Display the plot
print(p)
p+  # Set custom fill colors and labels
  annotate("text", x = 2, y = max(cohort$BMD), 
           label = paste("p-value =", round(p_value, 3)), 
           size = 5, color = "black")
ggsave("boxplot.eps", plot = p, device = "eps", width = 8, height = 6)

# Save the plot as PNG
ggsave("boxplot.png", plot = p, device = "png", width = 8, height = 6, dpi = 300, limitsize = FALSE)

