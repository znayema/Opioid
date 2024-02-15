
#miRNA associated with opioid use and BMD 
#model1-a miRNA~Opioid+covariates(Age,sex,BMI)
#model1-b miRNA~BMD+covariates(Age,sex,BMI)
#take the intersection from 1a and 1b
#linear regression for mirnas present in at least 10% of the samples but if less than that and more than 5% do logistic 

library(tidyverse)
library(dplyr)
library(VennDiagram)
library(BiocManager)
library(ggplot2)
library(pheatmap)
library(haven)

#load your analytic file
cohort<-read.csv("/work/larylab/NAYEMA/Opioid/data/analytic_cohort.csv", header=T)
cohort <- cohort[, -1] 
colnames(cohort)
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

#model function 
#liner regression 

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

#logistic regression

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

#BMD covariates 
BMDcov <- "~BMD+Age + Sex + BMI"

#Opioid covariates 
OPIOIDcov <- "~OPIOID_USERS + Age + Sex + BMI"


#BMD and Opioid results from linear model
BMDresults1 <- mlr(names = names1, cov = BMDcov, data = miRNA_pheno1, var = "BMD")
OPIOIDresults1 <- mlr(names = names1, cov = OPIOIDcov, data = miRNA_pheno1, var = "OPIOID_USERS")

# BMD and Opioid results from logistic model 
BMDresults2 <- mlrg(names = names2, cov = BMDcov, data = miRNA_pheno2, var = "BMD")
OPIOIDresults2 <- mlrg(names = names2, cov = OPIOIDcov, data = miRNA_pheno2, var = "OPIOID_USERS")



BMDresults1$model <- "linear"
BMDresults2$model <- "logistic"
BMDresults1$Presence <- ">10%"
BMDresults2$Presence <- "5-10%"


OPIOIDresults1$model <- "linear"
OPIOIDresults2$model <- "logistic"
OPIOIDresults1$Presence <- ">10%"
OPIOIDresults2$Presence <- "5-10%"

#filter based on significant p value

BMDresults1_p <- BMDresults1 %>%  filter(., variable == "BMD") %>% filter(., `Pr(>|t|)` < 0.05)
BMDresults2_p <- BMDresults2 %>%  filter(., variable == "BMD") %>% filter(., `Pr(>|z|)` < 0.05)

OPIOIDresults1_p <- OPIOIDresults1 %>%  filter(., variable == "OPIOID_USERS") %>% filter(., `Pr(>|t|)` < 0.05)
OPIOIDresults2_p <- OPIOIDresults2 %>%  filter(., variable == "OPIOID_USERS") %>% filter(., `Pr(>|z|)` < 0.05)

#rbind bmd results 
colnames(BMDresults1_p)[4] <- "t/z value"
colnames(BMDresults2_p)[4] <- "t/z value"
colnames(BMDresults1_p)[5] <- "P value"
colnames(BMDresults2_p)[5] <- "P value"
BMDMirna<-rbind(BMDresults1_p,BMDresults2_p)
BMDMirna <- BMDMirna[order(BMDMirna$`P value`), ]

#rbind opioid results 
colnames(OPIOIDresults1_p)[4] <- "t/z value"
colnames(OPIOIDresults2_p)[4] <- "t/z value"
colnames(OPIOIDresults1_p)[5] <- "P value"
colnames(OPIOIDresults2_p)[5] <- "P value"
OPIOIDMirna<-rbind(OPIOIDresults1_p,OPIOIDresults2_p)
OPIOIDMirna <- OPIOIDMirna[order(OPIOIDMirna$`P value`), ]


write.csv(BMDMirna, "/work/larylab/NAYEMA/Opioid/data/BMD_P_0.05.csv")
write.csv(OPIOIDMirna, "/work/larylab/NAYEMA/Opioid/data/OPIOID_P_0.05.csv")


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

write.csv(VOLCANOBMDMirna, "/work/larylab/NAYEMA/Opioid/data/VOLCANOBMDMirna.csv")
write.csv(VOLCANOOPIOIDMirna, "/work/larylab/NAYEMA/Opioid/data/VOLCANOOPIOIDMirna.csv")


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

write.csv(results1_p, "/work/larylab/NAYEMA/Opioid/data/INT_MIRNA~BMD+OPIOID.csv")
write.csv(results2_p, "/work/larylab/NAYEMA/Opioid/data/INT_MIRNA~BMD*OPIOID.csv")





#bmd as y variable

##model2-a BMD~MIRNA+covariates(Age,sex,BMI)
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

#Covariates

BMDcov <- "Age + Sex + BMI"

#linear for >10%
BMDresults1 <- mlr( var = "BMD",names = names1, cov = BMDcov, data = miRNA_pheno1)
BMDresults1$FDR <- p.adjust(p = BMDresults1$`Pr(>|t|)`, method = "BH", n = nrow(BMDresults1))
BMDresults1_p <- BMDresults1 %>% filter(., miRNA == variable) %>% filter(., `Pr(>|t|)` < 0.05)




# Model 2 only (log/binary)

BMDresults2 <- mlr( var = "BMD",names = as.factor(names2), cov = BMDcov, data = cohort)
BMDresults2$FDR <- p.adjust(p = BMDresults2$`Pr(>|t|)`, method = "BH", n = nrow(BMDresults2))
BMDresults2_p <- BMDresults2 %>% filter(., miRNA == variable) %>% filter(., `Pr(>|t|)` < 0.05)




BMDMirna<-rbind(BMDresults1_p,BMDresults2_p)
write.csv(BMDMirna, "/work/larylab/NAYEMA/Opioid/data/Y-BMD-P-0.05.csv")

#FOR VOLCANO

BMDresults1_p <- BMDresults1 %>% filter(., miRNA == variable) 
BMDresults2_p <- BMDresults2 %>% filter(., miRNA == variable) 

BMDMirna2<-rbind(BMDresults1_p,BMDresults2_p)
write.csv(BMDMirna2, "/work/larylab/NAYEMA/Opioid/data/VOLCANO-Y-BMD-P-0.05.csv")


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
p_values <- coefficients_table$p.value
fdr_values <- p.adjust(p_values, method = "BH")

# Add FDR values to the coefficients table
coefficients_table$fdr_value <- fdr_values



# Print the updated coefficients table with adjusted p-values
print(coefficients_table)
# Print the coefficients table
print(coefficients_table)
write.csv(coefficients_table, "/work/larylab/NAYEMA/Opioid/data/int_bmd*mirnaopioid+cov.csv", row.names = FALSE)

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
p_values <- coefficients_table$p.value
fdr_values <- p.adjust(p_values, method = "BH")

# Add FDR values to the coefficients table
coefficients_table$fdr_value <- fdr_values



# Print the updated coefficients table with adjusted p-values
print(coefficients_table)
# Print the coefficients table
print(coefficients_table)
write.csv(coefficients_table, "/work/larylab/NAYEMA/Opioid/data/int_bmd~mirna+opioid+cov.csv", row.names = FALSE)
