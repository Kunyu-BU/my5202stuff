

library(ggplot2)
library(readxl)
library(tidyverse)
library(palmerpenguins)
library(ggthemes)
library(scales)

exp <- read_excel("EXP4_averaged_with_SD.xlsx")
print (exp)

exp$treatment <- factor(exp$treatment, levels = c("C", "V 1X", "V 5X", "V 10X"))

ggplot(exp, aes(x = Day, y = mean_result, color = treatment, fill = treatment)) + 
geom_point(size = 2 ) + 
facet_grid(pigment ~ Host, scales = "free") + 
theme(text = element_text(size = 14)) + 
geom_smooth(method = "loess") + 
labs (title = "Amoeba Grazing EXP4",
subtitle = "D8",
x = "Days", y = "Pigment (RFU)",
color = "treatment", shape = "treatment"
) + scale_y_continuous(labels = comma) +
  geom_smooth(se = FALSE) +
  theme_minimal()


ls("package:base")
ls("package:dplyr")
length(ls("package:base"))
length(ls("package:dplyr"))

rm(list = ls())
getwd()



# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)

# Read the Excel file and the correct sheet
data <- read_excel("EXP 4.xlsx", sheet = "Pigments")

# Calculate average and standard deviation by group (excluding 'rep')
summary_data <- data %>%
  group_by(Day, Host, treatment, `pigment`) %>%
  summarise(
    mean_result = mean(Results, na.rm = TRUE),
    .groups = "drop"
  )

# Save the summarized data to a new Excel file
write_xlsx(summary_data, "EXP4_averaged_with_SD.xlsx")


# Load required libraries
library(readxl)
library(dplyr)
library(openxlsx)

# -----------------------------
# 1. Load and Clean the Data
# -----------------------------
df <- read_excel("EXP 4.xlsx", sheet = "Pigments")

# Clean up column names and values
df <- df %>%
  rename(pigment = `pigment (RFU)`) %>%
  mutate(
    treatment = trimws(treatment),
    treatment = recode(treatment, "V 1X" = "1X", "V 5X" = "5X", "V 10X" = "10X"),
    pigment = trimws(pigment),
    Host = trimws(Host)
  )

# -----------------------------
# 2. Run One-Way ANOVA by Host × Pigment
# -----------------------------
results <- data.frame()

for (host in unique(df$Host)) {
  for (pig in unique(df$pigment)) {
    sub_data <- df %>% filter(Host == host, pigment == pig)
    
    model <- aov(Results ~ treatment, data = sub_data)
    p_val <- summary(model)[[1]][["Pr(>F)"]][1]
    
    results <- rbind(results, data.frame(
      Host = host,
      Pigment = pig,
      p_value = round(p_val, 5),
      Significant = ifelse(p_val < 0.05, "Yes", "No")
    ))
  }
}

# View results
print(results)

# -----------------------------
# 3. Export to Excel
# -----------------------------
write.xlsx(results, "ANOVA_Treatment_by_Host_Pigment.xlsx", rowNames = FALSE)



# Load required libraries
library(readxl)
library(dplyr)
library(multcompView)
library(ggplot2)

# Load the data
df <- read_excel("EXP 4.xlsx")

# Clean up column names and values
df <- df %>%
  mutate(
    Host = trimws(Host),
    pigment = trimws(pigment),
    treatment = trimws(treatment)
  ) %>%
  filter(!is.na(Results))  # Remove rows with NA in Results

# Create an empty list to store results
tukey_results <- list()

# Loop through each Host and pigment combination
for (h in unique(df$Host)) {
  for (p in unique(df$pigment)) {
    
    # Subset data
    sub_df <- df %>%
      filter(Host == h, pigment == p)
    
    # Check if there are enough replicates
    if (length(unique(sub_df$treatment)) > 1 && min(table(sub_df$treatment)) > 1) {
      
      # One-way ANOVA
      model <- aov(Results ~ treatment, data = sub_df)
      
      # Tukey HSD post-hoc test
      tukey <- TukeyHSD(model)
      
      # Store results with identifiers
      res <- as.data.frame(tukey$treatment)
      res$Comparison <- rownames(res)
      res$Host <- h
      res$Pigment <- p
      
      tukey_results[[paste(h, p)]] <- res
    }
  }
}

# Combine all results into one data frame
final_tukey <- bind_rows(tukey_results)

# Rearrange columns
final_tukey <- final_tukey %>%
  select(Host, Pigment, Comparison, everything())

# Print final results
print(final_tukey)

# Optionally, export to Excel
write.csv(final_tukey, "TukeyHSD_Treatment_by_Host_Pigment.csv", row.names = FALSE)








install.packages("readxl")
install.packages("dplyr")
install.packages("writexl")

library(readxl)
library(dplyr)
library(writexl)


data <- read_excel("EXP 4.xlsx", sheet = 1)
head(data)



# Ensure the date is in correct format
data <- data %>%
  mutate(Date = as.Date(Day))

# Sort data and calculate daily variation
data_variation <- data %>%
  arrange(rep, Day) %>%
  group_by(rep) %>%
  mutate(Pigment_Variation = Results - lag(Results)) %>%
  ungroup()

avg_variation <- data_variation %>%
  group_by(rep) %>%
  summarise(Average_Daily_Variation = mean(Pigment_Variation, na.rm = TRUE))


write_xlsx(list(Daily_Variation = data_variation,
                Average_Variation = avg_variation),
           "Pigment_Variation_Results.xlsx")



library(readxl)
library(dplyr)
library(writexl)

# Load data
data <- read_excel("EXP 4.xlsx")

# Ensure correct types
data <- data %>%
  mutate(
    Day = as.numeric(day),
    rep = as.factor(rep),
    Results = as.numeric(results)
  )

# Calculate daily variation
data_variation <- data %>%
  arrange(host, treatment, pigment, rep, day) %>%
  group_by(host, treatment, pigment, rep) %>%
  mutate(Variation = results - lag(results)) %>%
  ungroup()

# Calculate average variation
avg_variation <- data_variation %>%
  group_by(host, treatment, pigment, rep) %>%
  summarise(Average_Variation = mean(Variation, na.rm = TRUE), .groups = "drop")

# Export results
write_xlsx(list(
  Daily_Variation = data_variation,
  Average_Variation = avg_variation
), "Pigment_Variation_Results.xlsx")
