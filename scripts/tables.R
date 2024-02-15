#TABLES
library(dplyr)
library(flextable)

getwd()


table1<-read.csv("OPIOID_P_0.05.csv",header=T)
colnames(table1)


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
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/tables/mirna~opioid-table1.docx")

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
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/tables/mirna~BMD-table2.docx")

#TABLE3

table3<-read.csv("Y-BMD-P-0.05.csv",header=T)

colnames(table3)



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
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/tables/BMD~mirna-table3.docx")

# table4

table4<-read.csv("int_bmd*mirnaopioid+cov.csv",header=T)

table4 <- table4[!table4$variable %in% c("Age", "SexMale", "BMI","(Intercept)"), ]




plotting_columns <- table4 %>% 
  mutate(p.value = round(p.value, digits = 3),
         Estimate = round(coefficient, digits = 3),
         SE = round(std.error, digits = 3),
         FDR = round(fdr_value, digits = 3)) %>% 
  select(variable, Estimate, SE, p.value,FDR)



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
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/tables/BMD~mirna*opioid-table4.docx")

#table 5

table5<-read.csv("int_bmd~mirna+opioid+cov.csv", header=T)

table5 <- table5[!table5$variable %in% c("Age", "SexMale", "BMI","(Intercept)"), ]

plotting_columns <- table5 %>% 
  mutate(p.value = round(p.value, digits = 3),
         Estimate = round(coefficient, digits = 3),
         SE = round(std.error, digits = 3),
         FDR = round(fdr_value, digits = 3))%>% 
  select(variable, Estimate, SE, p.value,FDR)



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
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/tables/BMD~mirna+opioid-table5.docx")

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
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/tables/mirna~BMD*opioid-table6.docx")

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
save_as_docx(flextable, path = "/work/larylab/NAYEMA/Opioid/results/tables/mirna~BMD+opioid-table7.docx")
