getwd()
setwd("C:/Users/kabalo/Desktop")
survey_data <- read.csv("DINING LOCATION PREFERENCES AMONG METU STUDENTS.csv", header = T)
head(survey_data)
colnames(survey_data)
colnames(survey_data) <- c(
  "Timestamp",
  "Academic_Year",
  "Age",
  "Gender", 
  "Living_Situation",
  "Daily_Spending", 
  "Family_DineOut_Freq",
  "Price_Importance", 
  "Proximity_Importance",
  "Variety_Importance", 
  "Quality_Importance", 
  "Hygiene_Importance", 
  "Ambiance_Importance", 
  "Service_Speed_Importance", 
  "Frequency_Makara", 
  "Frequency_Cati", 
  "Frequency_Ziyafet", 
  "Frequency_Central", 
  "Frequency_Susam", 
  "Frequency_BBQ_Station",
  "Frequency_Sunshine",
  "Frequency_Dormitory_Canteens", 
  "Frequency_Department_Canteens",
  "Frequency_METU_Dining_Hall", 
  "Frequency_Cankaya_House", 
  "Frequency_Off_Campus", 
  "Frequency_Food_Delivery", 
  "Satisfaction_Makara", 
  "Satisfaction_Cati", 
  "Satisfaction_Ziyafet",
  "Satisfaction_Central",
  "Satisfaction_Susam", 
  "Satisfaction_BBQ_Station",
  "Satisfaction_Sunshine", 
  "Satisfaction_Dormitory_Canteens", 
  "Satisfaction_Department_Canteens", 
  "Satisfaction_METU_Dining_Hall",
  "Satisfaction_Cankaya_House", 
  "Satisfaction_Off_Campus", 
  "Satisfaction_Food_Delivery", 
  "Improvements", 
  "Additional_Comments" 
)
library(ggplot2)
str(survey_data)
table(survey_data$Gender) 
table(survey_data$Hygiene_Importance)  
filtered_data <- survey_data[survey_data$Gender %in% c("Male", "Female"), ]
table(filtered_data$Gender)
ggplot(filtered_data, aes(x = Gender, y = Hygiene_Importance, fill = Gender)) +
  geom_boxplot() +
  labs(
    title = "Importance of Cleanliness and Hygiene by Gender",
    x = "Gender",
    y = "Hygiene Importance"
  ) +
  theme_minimal()

shapiro_test_female <- shapiro.test(filtered_data$Hygiene_Importance[filtered_data$Gender == "Female"])
shapiro_test_male <- shapiro.test(filtered_data$Hygiene_Importance[filtered_data$Gender == "Male"])

print(shapiro_test_female)
print(shapiro_test_male)
filtered_data$Gender <- as.factor(filtered_data$Gender)
levene_test <- car::leveneTest(Hygiene_Importance ~ Gender, data = filtered_data, center = median)
print(levene_test)
t_test_result <- t.test(Hygiene_Importance ~ Gender, 
                        data = filtered_data, 
                        var.equal = TRUE)
print(t_test_result)

str(survey_data$Living_Situation)
str(survey_data$Daily_Spending)
table(survey_data$Living_Situation)
table(survey_data$Daily_Spending)
spending_table <- table(survey_data$Living_Situation, survey_data$Daily_Spending)
contingency_table <- table(survey_data$Living_Situation, survey_data$Daily_Spending)
print(contingency_table)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)
simulated_chi_square_test <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 2000)
print(simulated_chi_square_test)
colors <- c("#FF9999", "#FFCC99", "#FFFF99", "#99FF99", "#99CCFF", "#CC99FF")
survey_data$Daily_Spending <- factor(survey_data$Daily_Spending, 
                                     levels = c("0-50 TL", "51-75 TL", "76-100 TL", 
                                                "101-150 TL", "151-200 TL", "201 TL and above"))
ggplot(survey_data, aes(x = Living_Situation, fill = Daily_Spending)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = colors) +
  labs(title = "Proportion of Daily Spending by Living Situation",
       x = "Living Situation", y = "Proportion", fill = "Daily Spending") +
  theme_minimal()
table(is.na(survey_data$Living_Situation))

summary(survey_data$Age)
summary(survey_data$Proximity_Importance)

filtered_data <- survey_data[survey_data$Age >= 18 & survey_data$Age <= 25, ]
filtered_data$Age_Group <- cut(
  filtered_data$Age,
  breaks = c(17, 21, 25),
  labels = c("18-21", "22-25"),
  right = TRUE
)

table(filtered_data$Age_Group)
summary(filtered_data$Proximity_Importance ~ filtered_data$Age_Group)
ggplot(filtered_data, aes(x = Age_Group, y = Proximity_Importance, fill = Age_Group)) +
  geom_boxplot() +
  labs(
    title = "Proximity Importance by Age Group",
    x = "Age Group",
    y = "Proximity Importance"
  ) +
  theme_minimal()

shapiro_18_21 <- shapiro.test(filtered_data$Proximity_Importance[filtered_data$Age_Group == "18-21"])
shapiro_22_25 <- shapiro.test(filtered_data$Proximity_Importance[filtered_data$Age_Group == "22-25"])

print(shapiro_18_21)
print(shapiro_22_25)
levene_test <- leveneTest(Proximity_Importance ~ Age_Group, data = filtered_data)
print(levene_test)
anova_result <- aov(Proximity_Importance ~ Age_Group, data = filtered_data)
summary(anova_result)
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

library(dplyr)
analysis_data <- survey_data %>%
  select(Daily_Spending, Frequency_METU_Dining_Hall)
analysis_data$Daily_Spending <- case_when(
  analysis_data$Daily_Spending == "0-50 TL" ~ 25,
  analysis_data$Daily_Spending == "51-100 TL" ~ 75,
  analysis_data$Daily_Spending == "101-150 TL" ~ 125,
  analysis_data$Daily_Spending == "151-200 TL" ~ 175,
  analysis_data$Daily_Spending == "201 TL and above" ~ 225,
  TRUE ~ NA_real_ 
)

analysis_data$Frequency_Group <- ifelse(
  analysis_data$Frequency_METU_Dining_Hall <= 3, "Low", "High"
)

analysis_data <- na.omit(analysis_data)
group_means <- analysis_data %>%
  group_by(Frequency_Group) %>%
  summarise(Mean_Spending = mean(Daily_Spending, na.rm = TRUE))

print(group_means)
t_test_result <- t.test(Daily_Spending ~ Frequency_Group, data = analysis_data)
print(t_test_result)


ggplot(group_means, aes(x = Frequency_Group, y = Mean_Spending, fill = Frequency_Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_text(aes(label = round(Mean_Spending, 1)), vjust = -0.5) +
  labs(
    title = "Mean Daily Spending by METU Dining Hall Usage Frequency",
    x = "Frequency Group",
    y = "Mean Spending (TL)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Low" = "#FF9999", "High" = "#99CCFF"))


living_data <- survey_data %>%
  select(Living_Situation, Frequency_Off_Campus)

living_data <- living_data %>%
  mutate(
    Accommodation_Type = ifelse(
      Living_Situation == "On-campus Dormitory (METU Dormitory)", "Dormitory", "Other"
    )
  )

living_data <- na.omit(living_data)

table(living_data$Accommodation_Type)
table(living_data$Frequency_Off_Campus)

wilcox_test_result <- wilcox.test(
  Frequency_Off_Campus ~ Accommodation_Type, 
  data = living_data,
  alternative = "less",
  exact = FALSE
)

print(wilcox_test_result)
ggplot(living_data, aes(x = Accommodation_Type, y = Frequency_Off_Campus, fill = Accommodation_Type)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7) + 
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "yellow") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Violin Plot: Off-Campus Dining Frequency by Accommodation Type",
    x = "Accommodation Type",
    y = "Off-Campus Dining Frequency"
  ) +
  theme_minimal()
