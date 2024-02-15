# GOAL: Build individual analytic file for opioid project

# DATA --------------------------------------------------------------------
# 
# Read all relevant data sets-Farmingham and are stored in dbgap
#data9 under dbgap has all the phenotype data 
#for phenotype data description check in /microsoft team/lary lab/files/larylab intro/fhsphenotypicdata_1_29_2021.xlsx
# *************************************************************************


rm(list=ls())
phenoPath = "/work/larylab/dbgap/"
library(dplyr)
library(tidyverse)
setwd(phenoPath)
common_cols <- c("DBGAP_SUBJECT_ID", "SHAREID", "IDTYPE")

#dates-includes all the exam dates (c1=cohort1, ccohort2)

examdate1 <- 'data9/phs000007.v33.pht003099.v8.p14.c1.vr_dates_2019_a_1175s.HMB-IRB-MDS.txt.gz'
examdate2 <-'data9/phs000007.v33.pht003099.v8.p14.c2.vr_dates_2019_a_1175s.HMB-IRB-NPU-MDS.txt.gz'

#covs-includes covariates information in each exam date

#offspring-we have mirna data of offspring from exam 8 so we are getting covariates info from exam 8

covoff1 <- 'data9/phs000007.v33.pht006027.v4.p14.c1.vr_wkthru_ex09_1_1001s.HMB-IRB-MDS.txt.gz'
covoff2 <-'data9/phs000007.v33.pht006027.v4.p14.c2.vr_wkthru_ex09_1_1001s.HMB-IRB-NPU-MDS.txt.gz'

#gen 3-mirna data avaiable in Exam 2 

covgen1<-'data9/phs000007.v33.pht006026.v5.p14.c1.vr_wkthru_ex03_3b_1191s.HMB-IRB-MDS.txt.gz'
covgen2<-'data9/phs000007.v33.pht006026.v5.p14.c2.vr_wkthru_ex03_3b_1191s.HMB-IRB-NPU-MDS.txt.gz'


#drug
#med info for offspring in exam 8

drugoff1 <- 'data9/phs000007.v33.pht000828.v8.p14.c1.meds1_8s.HMB-IRB-MDS.txt.gz'
drugoff2<-'data9/phs000007.v33.pht000828.v8.p14.c2.meds1_8s.HMB-IRB-NPU-MDS.txt.gz'

#med info for offspring in exam 2
druggen1<- 'data9/phs000007.v33.pht003098.v6.p14.c1.vr_meds_2011_m_0675s.HMB-IRB-MDS.txt.gz'
druggen2<-'data9/phs000007.v33.pht003098.v6.p14.c2.vr_meds_2011_m_0675s.HMB-IRB-NPU-MDS.txt.gz'

#miRNA- gen3 and offspring-data2 under dbgap

miRNA1<-'data2/phe000005.v9.FHS_SABRe_project4_miRNA.expression-data-matrixfmt.c1/l_mrna_2011_m_0797s_17_c1.csv.gz'
miRNA2<-'data2/phe000005.v9.FHS_SABRe_project4_miRNA.expression-data-matrixfmt.c2/l_mrna_2011_m_0797s_17_c2.csv.gz'



#BMD for offspring and gen3
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


#get exam2 and exam 8 dates 
exam_dates <- bind_rows(data_list[['examdate1']], data_list[['examdate2']]) %>%
  dplyr::select(matches(common_cols),AGE1,SEX,ATT2,ATT8,DATE2,DATE8,AGE2,AGE8) 

#covs
#offspring
#merging cov data with exam 8 data and calculating their age at exam 8
covoff <-bind_rows(data_list[['covoff1']], data_list[['covoff2']]) %>% 
  dplyr::select(matches(c(common_cols,'SEX','ATT8','AGE1','AGE8','BG8','BMI8','CALC_LDL8','CURRSMK8','DBP8',
                          'FASTING_BG8','CPD8','DMRX8','HRX8','LIPRX8','IDTYPE'))) %>%
  left_join(exam_dates)%>%rename(BG = BG8,BMI=BMI8,LDL=CALC_LDL8,CURRSMK=CURRSMK8,DBP=DBP8,FASTING_BG=FASTING_BG8,CPD=CPD8,DMRX=DMRX8,HRX=HRX8,
                                 LIPRX=LIPRX8)%>%
  mutate(EXAM_DATE = DATE8) %>%
  mutate(AGE = AGE1 + EXAM_DATE/365.25) %>%
  filter(!is.na(EXAM_DATE)) %>%
  dplyr::select(matches(common_cols),AGE,SEX,EXAM_DATE,BMI,BG,LDL,CURRSMK,CPD,DMRX,HRX)


#gen3
#merging cov data with exam 2 data and calculating their age at exam 2
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

#getting all the cov info for offspring and gen3
all_cov <- bind_rows(covoff,covgen)


#drugs
druggen<-bind_rows(data_list[['druggen1']], data_list[['drugogen2']])%>%
  dplyr::select(matches(c(common_cols,'IDTYPE','ATC_COD1','SYSTEM1','THER_GP1','THER_GP2','PHRM_GP1','CHEM_GP1','CHEM_NM1','MEDNAME','MEDPER','MEDPRN'))) 
drugoff<-bind_rows(data_list[['drugoff1']], data_list[['drugoff2']])%>%
  dplyr::select(matches(c(common_cols,'IDTYPE','ATC_COD1','SYSTEM1','THER_GP1','THER_GP2','PHRM_GP1','CHEM_GP1','CHEM_NM1','MEDNAME','MEDPER','MEDPRN'))) 

#getting all the drug info for offspring and gen3
all_drug<-bind_rows(drugoff,druggen)

#filtered only those really taking opioid 
OPIOID_drug<-bind_rows(drugoff,druggen)%>% 
  select(SHAREID,PHRM_GP1,THER_GP1,MEDNAME,MEDPRN,MEDPER,CHEM_GP1,CHEM_NM1) %>% 
  filter(PHRM_GP1=="OPIOIDS") %>% 
  distinct(SHAREID, .keep_all = TRUE)

#bmd
#gen3
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
#offspring
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

#combining gen3 and offspring bmd
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

#miRNA data format is not compatible so

#replaces all occurrences of dots (.) in the column names of miRNAdat with commas (,)
colnames(miRNAdat) <- gsub("\\.", ",", colnames(miRNAdat))

#the code takes the original data frame miRNAdat, separates each column based on commas, assigns new column names "Column 1", "Column 2", ..., 
miRNAdat2 <- miRNAdat  %>%
  separate(colnames(miRNAdat ), into = paste("Column", 1:416), sep = ",", remove = TRUE, convert = TRUE)


# Assuming split_column_names is a list of character vectors
split_column_names <- strsplit(colnames(miRNAdat), "\\,")

# Create a data frame with split names as rows
split_df <- as.data.frame(do.call(rbind, split_column_names))

colnames(miRNAdat2) <- colnames(split_df)
colnames(miRNAdat2) <- split_df[1, ]


miRNA_delta_cq <- miRNAdat2[-1]

#The absolute difference in cycle threshold (|ΔCq|) is calculated by subtracting the Cq value from 27 or |ΔCq| = −(Cq-27)
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

save(cohort, file = "/work/larylab/NAYEMA/Opioid/data/analytic_cohort.RData")
write.csv(cohort, file = "/work/larylab/NAYEMA/Opioid/data/analytic_cohort.csv")


##Summary table

library(gtsummary)

trial2 <- cohort %>% select(OPIOID_USERS,Age, Sex, BMI, BMD, MEDPRN,MEDPER, CHEM_GP1, CHEM_NM1)

table3<-trial2 %>%
  tbl_summary(by = OPIOID_USERS,missing_text = "(Missing)") %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Main Summary**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Table 1.Cohort summary**") %>%
  bold_labels()

#Cross table
cross_table1 <- tbl_cross(
  cohort,
  row = MEDPER,
  col = MEDPRN,
  missing = "always",margin = NULL,missing_text = "(Missing)"
)


cross_table2 <- tbl_cross(
  cohort,
  row = CHEM_GP1,
  col = MEDPRN,missing = "always",margin = NULL,missing_text = "(Missing)"
)

cross_table3 <- tbl_cross(
  cohort,
  row = CHEM_NM1,
  col = MEDPRN,missing = "always",margin = NULL,missing_text = "(Missing)"
)
combined_rows <- tbl_stack(
  tbls = list(cross_table1, cross_table2, cross_table3)
)
#Final table 1
table4 <- tbl_merge(list(table3, combined_rows), tab_spanner = c("**Main Summary**", "**MEDPRN**"))

#save as html
table4%>%
  as_gt() %>%
  gt::gtsave(filename = "/work/larylab/NAYEMA/Opioid/results/tables/Cohort_summary_table.html")

library(flextable)
library(officer)


#save as doc file 
# Convert table3 to a flextable object
table4_flex <- as_flex_table(table4)
table4_flex <- fontsize(table4_flex, size = 7.5)


# Create a Word document
doc <- read_docx()

# Add the flextable to the document
doc <- doc %>%
  body_add_flextable(
    value = table4_flex,
    align = "center"
  )

# Save the Word document
filename <- "/work/larylab/NAYEMA/Opioid/results/tables/Cohort_summary_table.docx"
print(filename)
print(doc, target = filename)
