library(tidyverse)
library(plotly)
library(beepr)
library(survey)
library(scales)
library(reshape2)

# In this script I produce a series of charts that illustrate how obesity rate changes in various scenarios and for subgroups 


# Read flat file Health Survey data

df <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\calorie_deficit_scenarios_w_imd.csv", sep = ",", fileEncoding="UTF-8-BOM")

# Create grouped age groups

df <- df %>% 
  mutate(Age_group_2 = cut(Age_est, 
                           breaks = c(16, 20, 30, 40, 50, 60, 200), 
                           right = F,
                           labels = c("16-19", "20-29", "30-39", "40-49", "50-59", "60 +")))

# Declare survey design

hse <- svydesign(id = ~X,
          weights = ~sample_weight,
          data = df)

# Calculate obesity rate

tab <- svytable(~BMI_class, design = hse)

# Add proportions to table
tab_w <- tab %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab_w$BMI_class <- factor(tab_w$BMI_class, levels = c("underweight", "normal", "overweight", "obese"))

# Create a barplot
ggplot(data = tab_w, mapping = aes(x = BMI_class, y = Prop)) + 
  geom_col(fill = "blue") +
  geom_text(aes(label = label_percent()(Prop)), vjust = -0.3, size = 3.5) +
  theme_bw() +
  labs(x = "", y = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,0.7))

# subgroup analysis

# gender

tab_gender <- svytable(~BMI_class + Sex, design = hse)

# Add proportions to table
tab_w_gender <- tab_gender %>%
  as.data.frame() %>%
  group_by(BMI_class) %>% 
  mutate(Prop = (Freq/sum(Freq))) %>% 
  mutate(label_y = cumsum(Prop) - 0.5 * Prop)


# comparison

tab_gender_all <- svytable(~Sex, design = hse)

tab_w_gender_all <- tab_gender_all %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq))) %>% 
  mutate(label_y = cumsum(Prop) - 0.5 * Prop) %>% 
  mutate(BMI_class = "All")

tab_w_gender_final <- rbind(tab_w_gender, tab_w_gender_all)

tab_w_gender_final$BMI_class <- factor(tab_w_gender_final$BMI_class, levels = c("underweight", "normal", "overweight", "obese", "All"))


# Create a barplot
ggplot(data = tab_w_gender_final, mapping = aes(x = BMI_class, y = Prop, fill = Sex)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = label_percent(1)(Prop)), size = 3.5) +
  theme_bw() +
  labs(x = "", y = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1))

# ethnicity

tab_ethn <- svytable(~BMI_class + Ethnicity, design = hse)

# Add proportions to table
tab_w_ethn <- tab_ethn %>%
  as.data.frame() %>%
  group_by(BMI_class) %>% 
  mutate(Prop = (Freq/sum(Freq)))

# comparison

tab_ethn_all <- svytable(~Ethnicity, design = hse)

tab_w_ethn_all <- tab_ethn_all %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq))) %>% 
  mutate(BMI_class = "All")

tab_w_ethn_final <- rbind(tab_w_ethn, tab_w_ethn_all)

tab_w_ethn_final$BMI_class <- factor(tab_w_ethn_final$BMI_class, levels = c("underweight", "normal", "overweight", "obese", "All"))

tab_w_ethn_final$Ethnicity <- factor(tab_w_ethn_final$Ethnicity, levels = c("Asian", "Black", "White", "Mixed", "Other"))

tab_w_ethn_final <- tab_w_ethn_final %>% 
  group_by(BMI_class) %>% 
  arrange(Ethnicity) %>% 
  mutate(label_y = 1 - (cumsum(Prop) - 0.5 * Prop))

# Create a barplot
ggplot(data = tab_w_ethn_final, mapping = aes(x = BMI_class, y = Prop, fill = Ethnicity)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = label_percent(1)(Prop)), size = 3.5) +
  theme_bw() +
  labs(x = "", y = "")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1))

# Age group

tab_age <- svytable(~BMI_class + Age_group_2, design = hse)

# Add proportions to table
tab_w_age <- tab_age %>%
  as.data.frame() %>%
  group_by(BMI_class) %>% 
  mutate(Prop = (Freq/sum(Freq)))

# comparison

tab_age_all <- svytable(~Age_group_2, design = hse)

tab_w_age_all <- tab_age_all %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq))) %>% 
  mutate(BMI_class = "All")

tab_w_age_final <- rbind(tab_w_age, tab_w_age_all)

tab_w_age_final$BMI_class <- factor(tab_w_age_final$BMI_class, levels = c("underweight", "normal", "overweight", "obese", "All"))

tab_w_age_final <- tab_w_age_final %>% 
  group_by(BMI_class) %>% 
  arrange(Age_group_2) %>% 
  mutate(label_y = 1 - (cumsum(Prop) - 0.5 * Prop))

# Create a barplot
ggplot(data = tab_w_age_final, mapping = aes(x = BMI_class, y = Prop, fill = Age_group_2)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = label_percent(1)(Prop)), size = 3.5) +
  theme_bw() +
  labs(x = "", y = "", fill = "Age group")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1))

# Deprivation

tab_imd <- svytable(~BMI_class + IMD_q, design = hse)

# Add proportions to table
tab_w_imd <- tab_imd %>%
  as.data.frame() %>%
  group_by(BMI_class) %>% 
  mutate(Prop = (Freq/sum(Freq)))

# comparison

tab_imd_all <- svytable(~IMD_q, design = hse)

tab_w_imd_all <- tab_imd_all %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq))) %>% 
  mutate(BMI_class = "All")

tab_w_imd_final <- rbind(tab_w_imd, tab_w_imd_all)

tab_w_imd_final$BMI_class <- factor(tab_w_imd_final$BMI_class, levels = c("underweight", "normal", "overweight", "obese", "All"))

tab_w_imd_final <- tab_w_imd_final %>% 
  group_by(BMI_class) %>% 
  arrange(IMD_q) %>% 
  mutate(label_y = 1 - (cumsum(Prop) - 0.5 * Prop))

# Create a barplot
ggplot(data = tab_w_imd_final, mapping = aes(x = BMI_class, y = Prop, fill = IMD_q)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = label_y, label = label_percent(1)(Prop)), size = 3.5) +
  theme_bw() +
  labs(x = "", y = "", fill = "IMD quintile")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1))

# scenario 1

sc1 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_3years_90kcal.csv")

# Declare survey design

sc1_ds <- svydesign(id = ~X,
                 weights = ~sample_weight,
                 data = sc1)

# Calculate obesity rate

tab1 <- svytable(~final_BMI_class, design = sc1_ds)

# Add proportions to table
tab1_w <- tab1 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab1_w$final_BMI_class <- factor(tab1_w$final_BMI_class, levels = c("underweight", "normal", "overweight", "obese"))

# Create a barplot
ggplot(data = tab1_w, mapping = aes(x = final_BMI_class, y = Prop)) + 
  geom_col(fill = "blue") +
  geom_text(aes(label = label_percent(1)(Prop)), vjust = -0.3, size = 3.5) +
  theme_minimal() +
  labs(x = "", y = "%")

# Combine before and after

tab_all <- rbind(tab_w %>% 
                   mutate(type = "Now") %>% 
                   rename(BMI = BMI_class),
                 tab1_w %>% 
                   mutate(type = "90 kcal reduction") %>% 
                   rename(BMI = final_BMI_class))

tab_all$type <- factor(tab_all$type, levels = c("Now", "90 kcal reduction"))

# Create a barplot
ggplot(data = tab_all, 
       mapping = aes(x = BMI, 
                     y = Prop, 
                     fill = type, 
                     group = type)) + 
  geom_bar(stat="identity", 
           width=.9, 
           position = "dodge") +
  geom_text(aes(label = label_percent()(Prop)), 
            vjust = -0.3, 
            size = 3.5, 
            position = position_dodge(.9)) +
  theme_bw() +
  labs(x = "", 
       y = "%", 
       fill = " ") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,0.7))


# scenario 3

sc3 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_3.csv")

# chart distribution

sc3 %>% 
  select(id, baseIntake, newIntake, Sex) %>% 
  melt(., id.vars = c("id", "Sex")) %>% 
  mutate(intake_type = ifelse(variable == "baseIntake", "Now", "In 3 Years")) %>% 
  ggplot(., aes(x = value, group = intake_type, fill = intake_type)) +
  geom_density(alpha = 0.5) + 
  facet_wrap( ~ Sex) +
  theme_minimal() +
  labs(fill = "Daily Intake",
       x = "Daily kcal")

sc3_m <- merge(df, sc3[,c("id", "final_BMI_class")], by.x = "X", by.y = "id", all = T) %>% 
  mutate(final_BMI_class = ifelse(is.na(final_BMI_class), BMI_class, final_BMI_class))

# Declare survey design

sc3_ds <- svydesign(id = ~X,
                    weights = ~sample_weight,
                    data = sc3_m)

# Calculate obesity rate

tab1 <- svytable(~final_BMI_class, design = sc3_ds)

# Add proportions to table
tab1_w <- tab1 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab1_w$final_BMI_class <- factor(tab1_w$final_BMI_class, levels = c("underweight", "normal", "overweight", "obese"))


# Combine before and after

tab_all <- rbind(tab_w %>% 
                   mutate(type = "Now") %>% 
                   rename(BMI = BMI_class),
                 tab1_w %>% 
                   mutate(type = "Scenario 3") %>% 
                   rename(BMI = final_BMI_class))

tab_all$type <- factor(tab_all$type, levels = c("Now", "Scenario 3"))

# Create a barplot
ggplot(data = tab_all, 
       mapping = aes(x = BMI, 
                     y = Prop, 
                     fill = type, 
                     group = type)) + 
  geom_bar(stat="identity", 
           width=.9, 
           position = "dodge") +
  geom_text(aes(label = scales::label_percent(2)(Prop)), 
            vjust = -0.3, 
            size = 3.5, 
            position = position_dodge(.9)) +
  theme_bw() +
  labs(x = "", 
       y = "%", 
       fill = " ") +
  scale_y_continuous(labels = scales::label_percent(2)) +
  coord_cartesian(ylim = c(0,0.7))

# scenario 4

sc4 <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_modelling_scenario_4.csv")

# chart distribution

sc4 %>% 
  select(id, baseIntake, newIntake, Sex) %>% 
  melt(., id.vars = c("id", "Sex")) %>% 
  mutate(intake_type = ifelse(variable == "baseIntake", "Now", "In 3 Years")) %>% 
  ggplot(., aes(x = value, group = intake_type, fill = intake_type)) +
  geom_density(alpha = 0.5) + 
  facet_wrap( ~ Sex) +
  theme_minimal() +
  labs(fill = "Daily Intake",
       x = "Daily kcal")

sc4_m <- merge(df, sc4[,c("id", "final_BMI_class")], by.x = "X", by.y = "id", all = T) %>% 
  mutate(final_BMI_class = ifelse(is.na(final_BMI_class), BMI_class, final_BMI_class))

# Declare survey design

sc4_ds <- svydesign(id = ~X,
                    weights = ~sample_weight,
                    data = sc4_m)

# Calculate obesity rate

tab1 <- svytable(~final_BMI_class, design = sc4_ds)

# Add proportions to table
tab1_w <- tab1 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab1_w$final_BMI_class <- factor(tab1_w$final_BMI_class, levels = c("underweight", "normal", "overweight", "obese"))


# Combine before and after

tab_all <- rbind(tab_w %>% 
                   mutate(type = "Now") %>% 
                   rename(BMI = BMI_class),
                 tab1_w %>% 
                   mutate(type = "Scenario 4") %>% 
                   rename(BMI = final_BMI_class))

tab_all$type <- factor(tab_all$type, levels = c("Now", "Scenario 4"))

# Create a barplot
ggplot(data = tab_all, 
       mapping = aes(x = BMI, 
                     y = Prop, 
                     fill = type, 
                     group = type)) + 
  geom_bar(stat="identity", 
           width=.9, 
           position = "dodge") +
  geom_text(aes(label = scales::label_percent(2)(Prop)), 
            vjust = -0.3, 
            size = 3.5, 
            position = position_dodge(.9)) +
  theme_bw() +
  labs(x = "", 
       y = "%", 
       fill = " ") +
  scale_y_continuous(labels = scales::label_percent(2)) +
  coord_cartesian(ylim = c(0,0.7))


# scenario 2
##################################################################################################################
# this was run on the cloud so needs tweaking



sc2 <- read_csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\scenario2_agg_results.csv")

sc2 %>% 
  select(BMI_est, final_BMI_10p, final_BMI_15p, final_BMI_20p, final_BMI_22.5p) %>% 
  melt(.) %>% 
  mutate(BMI_type = ifelse(variable != "BMI_est",
                           gsub("final_BMI_", " ", variable),
                           "Now")) %>% 
  mutate(label = gsub("p", "% reduction", BMI_type)) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  geom_density(size = 1) + 
  geom_vline(xintercept = c(25,30), linetype = "dashed") +
  theme_bw() +
  xlim(10,55) +
  labs(color = "",
       x = "BMI")


# Declare survey design

sc2_ds <- svydesign(id = ~index,
                    weights = ~sample_weight,
                    data = sc2)

# Calculate current obesity rate

tab1 <- svytable(~BMI_class, design = sc2_ds)

# Add proportions to table
tab1_w <- tab1 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab1_w$final_BMI_class <- factor(tab1_w$BMI_class, levels = c("underweight", "normal", "overweight", "obese"))

# Calculate  obesity rate for scenarios

tab10 <- svytable(~final_BMI_class_10p, design = sc2_ds)

# Add proportions to table
tab10_w <- tab10 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab10_w$final_BMI_class <- factor(tab10_w$final_BMI_class_10p, levels = c("underweight", "normal", "overweight", "obese"))

tab15 <- svytable(~final_BMI_class_15p, design = sc2_ds)

# Add proportions to table
tab15_w <- tab15 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab15_w$final_BMI_class <- factor(tab15_w$final_BMI_class_15p, levels = c("underweight", "normal", "overweight", "obese"))

tab20 <- svytable(~final_BMI_class_20p, design = sc2_ds)

# Add proportions to table
tab20_w <- tab20 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab20_w$final_BMI_class <- factor(tab20_w$final_BMI_class_20p, levels = c("underweight", "normal", "overweight", "obese"))

tab22.5 <- svytable(~final_BMI_class_22.5p, design = sc2_ds)

# Add proportions to table
tab22.5_w <- tab22.5 %>%
  as.data.frame() %>%
  mutate(Prop = (Freq/sum(Freq)))

tab22.5_w$final_BMI_class <- factor(tab22.5_w$final_BMI_class_22.5p, levels = c("underweight", "normal", "overweight", "obese"))


# Combine before and after

tab_all <- rbind(tab1_w %>% 
                   mutate(type = "Now") %>% 
                   rename(BMI = BMI_class) %>% 
                   select(BMI, Freq, Prop, type),
                 tab10_w %>% 
                   mutate(type = "10% reduction") %>% 
                   rename(BMI = final_BMI_class) %>% 
                   select(BMI, Freq, Prop, type),
                 tab15_w %>% 
                   mutate(type = "15% reduction") %>% 
                   rename(BMI = final_BMI_class) %>% 
                   select(BMI, Freq, Prop, type),
                 tab20_w %>% 
                   mutate(type = "20% reduction") %>% 
                   rename(BMI = final_BMI_class) %>% 
                   select(BMI, Freq, Prop, type),
                 tab22.5_w %>% 
                   mutate(type = "22.5% reduction") %>% 
                   rename(BMI = final_BMI_class) %>% 
                   select(BMI, Freq, Prop, type))

tab_all$BMI <- factor(tab_all$BMI, levels = c("underweight", "normal", "overweight", "obese"))

tab_all$type <- factor(tab_all$type, levels = c("Now", "10% reduction", "15% reduction", "20% reduction", "22.5% reduction"))

# Create a barplot
ggplot(data = tab_all, 
       mapping = aes(x = BMI, 
                     y = Prop, 
                     fill = type, 
                     group = type)) + 
  geom_bar(stat="identity", 
           width=.9, 
           position = "dodge") +
  geom_text(aes(label = label_percent()(Prop)), 
            vjust = -0.3, 
            size = 3.5, 
            position = position_dodge(.9)) +
  theme_bw() +
  labs(x = "", 
       y = "%", 
       fill = " ") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,0.7))

