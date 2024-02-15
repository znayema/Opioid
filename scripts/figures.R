
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
library(ggrepel)

setwd("/work/larylab/NAYEMA/Opioid/data/")

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

ggsave("/work/larylab/NAYEMA/Opioid/results/figures/mirna-y variable/volcano_plot_opioid_microRNA.png", plot = Endpoint2, width = 10, height = 7, dpi = 400)


#BMD VOLCANO (mirna-y)
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

ggsave("/work/larylab/NAYEMA/Opioid/results/figures/mirna-y variable/volcano_plot_bmd_microRNA.png", plot = Endpoint2, width = 10, height = 7, dpi = 400)



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

ggsave("/work/larylab/NAYEMA/Opioid/results/figures/bmd-y variable/volcano_plot_bmd(y)_microRNA.png", plot = Endpoint2, width = 10, height = 7, dpi = 400)


#ven diagram

library(VennDiagram)
grid.newpage()


# Create the Venn diagram with number of mirnas significantly associated with BMD and Opioid 

venn_plot <- draw.pairwise.venn(
  area1 = 64, area2 = 28, cross.area = 10,
  category = c("BMD miRNA", "Opioid miRNA"),
  border = "black",
  lty = rep("blank", 2),
  fill = c(alpha("#440154ff", 0.3), alpha("#21908dff", 0.3)),
  alpha = rep(0.5, 2),
  cat.pos = c(0, 15),  # Adjust the position of the "Opioid miRNA" category label to the left
  cat.dist = rep(0.025, 2.5),
  cat.cex = 2.1,
  cex.num = 20,
  cat.col = c("#440154ff", '#21908dff')
)

png(file = "/work/larylab/NAYEMA/Opioid/results/figures/mirna-y variable/venn_diagram.png", width = 800, height = 600, units = "px", pointsize = 20, bg = "white")
grid.draw(venn_plot)
dev.off()

## Create the Venn diagram with number of mirnas significantly associated with BMD as y variable and Opioid as x variable
venn_plot <- draw.pairwise.venn(
  area1 = 65, area2 = 28, cross.area = 10,
  category = c("BMD miRNA", "Opioid miRNA"),
  border = "black",
  lty = rep("blank", 2),
  fill = c(alpha("#440154ff", 0.3), alpha("#21908dff", 0.3)),
  alpha = rep(0.5, 2),
  cat.pos = c(0, 15),  # Adjust the position of the "Opioid miRNA" category label to the left
  cat.dist = rep(0.025, 2.5),
  cat.cex = 2.1,
  cex.num = 20,
  cat.col = c("#440154ff", '#21908dff')
)

png(file = "/work/larylab/NAYEMA/Opioid/results/figures/bmd-y variable/venn_diagram_bmd(y).png", width = 800, height = 600, units = "px", pointsize = 20, bg = "white")
grid.draw(venn_plot)
dev.off()

#Grouped bar plot 


df<-read.csv("BMD_P_0.05.csv", header=T)
df2<-read.csv("OPIOID_P_0.05.csv", header=T)

BMDall <- c(as.character(df$miRNA))
OPIOIDall <- c(as.character(df2$miRNA))

int<- intersect(BMDall, OPIOIDall)

#for plot where mirna is y variable 
int_miRNAsbmd <- subset(df, miRNA %in% int)
int_miRNAsopioid <- subset(df2, miRNA %in% int)

#for plot where bmd is y variable
df3<-read.csv("Y-BMD-P-0.05.csv", header=T)
int_miRNAs3 <- subset(df3, miRNA %in% int)

#merge int mirna single model info associated with both bmd and opioid for forest plot
merged_int_miRNAs <- merge(int_miRNAsbmd, int_miRNAsopioid, by = "miRNA")
merged_int_miRNAs1 <- merged_int_miRNAs[, c("miRNA", "Estimate.x", "Estimate.y","Std..Error.x","Std..Error.y")]
write.csv(merged_int_miRNAs1, "/work/larylab/NAYEMA/Opioid/data/Forest.csv", row.names = FALSE)

#same work just bmd as y variable values 
merged_int_miRNAs2 <- merge(int_miRNAsopioid, int_miRNAs3, by = "miRNA")
merged_int_miRNAs3 <- merged_int_miRNAs2[, c("miRNA", "Estimate.x", "Estimate.y","Std..Error.x","Std..Error.y")]
write.csv(merged_int_miRNAs3, "/work/larylab/NAYEMA/Opioid/data/YForest.csv", row.names = FALSE)



library(ggplot2)

# read data
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


# Save the forest plot as a PNG with high resolution
ggsave("/work/larylab/NAYEMA/Opioid/results/figures/mirna-y variable/common_mirna_groupped_barplot.png", plot = grouped_bar_plot, width = 10, height = 7, dpi = 300)


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



# Save the forest plot as a PNG with high resolution
ggsave("/work/larylab/NAYEMA/Opioid/results/figures/bmd-y variable/common_mirna_groupped_bar_plot_bmdy.png", plot = grouped_bar_plot, width = 10, height = 7, dpi = 300)



##forest plot another style

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
    legend.background = element_rect(fill = "white", color = "white"), # White background for legend
    plot.background = element_rect(fill = "white"), # White background for plot area
    panel.background = element_rect(fill = "white"), # White background for panel area
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


# Save the forest plot as a PNG with high resolution
ggsave("/work/larylab/NAYEMA/Opioid/results/figures/mirna-y variable/int_mirna_forest_plot.png", plot = forest_plot, width = 10, height = 7, dpi = 300)


#another forest plot forbmd as y variable 
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
    legend.background = element_rect(fill = "white", color = "white"), # White background for legend
    plot.background = element_rect(fill = "white"), # White background for plot area
    panel.background = element_rect(fill = "white"), # White background for panel area
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



# Save the forest plot as a PNG with high resolution
ggsave("/work/larylab/NAYEMA/Opioid/results/figures/bmd-y variable/int_mirna_forest_plot(y).png", plot = forest_plot, width = 10, height = 7, dpi = 300)


###emmeans p value / box plot

library(emmeans)
library(ggplot2)

model <- lm(BMD ~ OPIOID_USERS+Age+Sex + BMI, data = cohort)
summary(model)
# Obtain emmeans
emmeans_values <- emmeans(model, "OPIOID_USERS")

# Convert emmeans values to a data frame
emmeans_df <- as.data.frame(emmeans_values)


p_value <- summary(model)$coefficients["OPIOID_USERS", "Pr(>|t|)"]

emmeans_plot_centered_rds <- ggplot(emmeans_df, aes(x = factor(OPIOID_USERS, labels = c("Non Users", "Users")), y = emmean)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), fill = "blue", width = 0.3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(width = 0.5), width = 0.2) +
  labs(x = "Opioid Users", y = "Mean BMD", title = "Mean BMD between Non Users and Users") +
  geom_text(aes(x = 1.5, y = max(emmeans_df$emmean) + 0.2, label = paste("p =", format(p_value, scientific = TRUE, digits = 2))),
            hjust = 0.5, size = 6) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"), # White background
    panel.background = element_rect(fill = "white"), # White background for panel area
    legend.background = element_rect(fill = "white"), # White background for legend
    axis.text = element_text(size = 18, color = "black"), # Size and color of axis text
    axis.title = element_text(size = 18, face = "bold", color = "black"), # Size, color, and boldness of axis title
    plot.title = element_text(size = 18, face = "bold", color = "black", margin = margin(0, 0, 20, 0), hjust = 0.5), # Size, color, boldness, and margin of plot title
    plot.caption = element_text(size = 8, color = "black"), # Size and color of plot caption
    legend.text = element_text(size = 8, color = "black"), # Size and color of legend text
    legend.title = element_text(size = 10, face = "bold", color = "black", margin = margin(0, 0, 20, 0)), # Size, color, boldness, and margin of legend title
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5), # Panel border settings
    legend.position = "none" # Remove legend
  )
ggsave("/work/larylab/NAYEMA/Opioid/results/figures/emmeans_plot_BMD_OPIOID.png", plot = emmeans_plot_centered_rds, width = 10, height = 7, dpi = 300)

# Create the box plot using ggplot2 with updated labels
cohort
p <- ggplot(cohort, aes(x = OPIOID_USERS, y = BMD, fill = OPIOID_USERS)) +
  geom_boxplot(width = 0.5, color = "black", alpha = 0.8) +
  labs(x = "Opioid Users", y = "BMD",
       title = paste("Box Plot of BMD by Opioid Users"),
       fill = "Opioid Users") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"), # White background
         # White background for panel area
        legend.background = element_rect(fill = "white"),legend.title = element_text(size = 16, face = "bold"),  # Set legend title size and make it bold
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

ggsave("/work/larylab/NAYEMA/Opioid/results/figures/boxplot.eps", plot = p, device = "eps", width = 8, height = 6)

# Save the plot as PNG
ggsave("/work/larylab/NAYEMA/Opioid/results/figures/boxplot.png", plot = p, device = "png", width = 8, height = 6, dpi = 300, limitsize = FALSE)


