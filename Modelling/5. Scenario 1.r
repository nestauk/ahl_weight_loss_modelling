rm(list = ls())

library(Hmisc)
library(lubridate)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(reshape2)
library(survey)
library(networkD3)

# read file and add under weight

under <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse2019_clean_no_outliers.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  filter(bmi_class == "underweight") %>% 
  mutate(finalWeight = wtval,
         startWeight = wtval,
         height = htval,
         age = age_est,
         Sex_letter = ifelse(sex_label == "female", "F", "M"),
         sex = Sex_letter,
         baseIntake = ifelse(sex == "M", -.0971*(wtval^2) + 40.853*wtval + 323.59, .0278*(wtval^2) + 9.2893*wtval + 1528.9),
         newIntake = baseIntake,
         TEE_final = baseIntake,
         weight_final = wtval,
         intake_final = newIntake,
         bmi_final = bmival,
         value = 0,
         target = wtval,
         calRed = 0)

full <- plyr::rbind.fill(under, read_csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_full_weight_loss.csv")) %>% 
  mutate(bmi_class = case_when(bmival <= 18.5 ~ "underweight",
                               bmival > 18.5 & bmival < 25 ~ "normal",
                               bmival >= 25 & bmival < 30 ~ "overweight",
                               bmival >= 30 & bmival < 40 ~ "obese",
                               bmival >= 40 ~ "morb obese",
                               TRUE ~ "NA")) %>% 
  mutate(final_BMI_class = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                     bmi_final >= 40 ~ "morb obese",
                                     TRUE ~ "NA"))

rbind(full %>% dplyr::select(bmi_class, sex, baseIntake, wt_int) %>% mutate(type = "base") %>% rename(intake = baseIntake),
      full %>% dplyr::select(bmi_class, sex, intake_final, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_final)) %>% 
  group_by(bmi_class, sex, type) %>% 
  summarise(intakeM = wtd.mean(intake, weight = wt_int)) %>% 
  dcast(., bmi_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = diff/base*100)


# obese only

obese_df <- read_csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse_obese_weight_loss.csv") %>% 
  mutate(bmi_class = case_when(bmival <= 18.5 ~ "underweight",
                               bmival > 18.5 & bmival < 25 ~ "normal",
                               bmival >= 25 & bmival < 30 ~ "overweight",
                               bmival >= 30 & bmival < 40 ~ "obese",
                               bmival >= 40 ~ "morb obese",
                               TRUE ~ "NA")) %>% 
  mutate(final_BMI_class = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                     bmi_final >= 40 ~ "morb obese",
                                     TRUE ~ "NA"))

rbind(obese_df %>% dplyr::select(bmi_class, sex, baseIntake, wt_int) %>% mutate(type = "base") %>% rename(intake = baseIntake),
      obese_df %>% dplyr::select(bmi_class, sex, intake_final, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_final)) %>% 
  group_by(bmi_class, sex, type) %>% 
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., bmi_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))

set.seed(3)

random <- full %>% sample_n(25)

write_csv(random, "/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/random_selection_full.csv")


# descriptives for the difference

obese_df %>% 
  mutate(diff = intake_final - baseIntake) %>% 
  group_by(bmi_class, sex) %>% 
  summarise(std = round(sqrt(wtd.var(diff, weight = wt_int)),1),
            min = min(diff),
            max = max(diff),
            perc25 = wtd.quantile(diff, probs = 0.25, weight = wt_int),
            perc75 = wtd.quantile(diff, probs = 0.75, weight = wt_int),
            iqr = perc75 - perc25)

obese_df %>% 
  group_by(sex) %>% 
  summarise(std = round(sqrt(wtd.var(calRed, weight = wt_int)),1),
            min = min(calRed),
            max = max(calRed),
            perc25 = wtd.quantile(calRed, probs = 0.25, weight = wt_int),
            perc75 = wtd.quantile(calRed, probs = 0.75, weight = wt_int),
            iqr = perc75 - perc25)

obese_df %>% 
  group_by(bmi_class, sex) %>% 
  summarise(year_avg = days(round(mean(day_final),0)) %/% years(1),
            months_avg = days(round(mean(day_final),0)) %% years(1) %/% months(1),
            year_max = days(round(max(day_final),0)) %/% years(1),
            months_max = days(round(max(day_final),0)) %% years(1) %/% months(1),
            year_min = days(round(min(day_final),0)) %/% years(1),
            months_min = days(round(min(day_final),0)) %% years(1) %/% months(1)) %>% 
  arrange(sex)


dclus2 <- svydesign(id=~X, weights = ~wt_int, data=full)

funDiff <- function(bmi_class_c, sex_c){
    sub <- subset(dclus2, sex == sex_c & bmi_class == bmi_class_c)
    tt<-svyttest(I(intake_final - baseIntake)~0, sub)
    return(data.frame(sex = sex_c, bmi_class = bmi_class_c, confint(tt, level=0.95)))
    }

list <- expand.grid(sex_c = unique(obese_df$sex), bmi_class_c = c("normal","overweight", "obese", "morb obese"))

pmap_dfr(list, funDiff)  %>% arrange(sex)

# Intake distribution

obese_df %>% 
  mutate(sex = ifelse(sex_label == "female", "Females", "Males")) %>% 
  dplyr::select(baseIntake, intake_final, sex, wt_int, bmi_class) %>% 
  melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
  mutate(label = ifelse(variable == "baseIntake", "Current", "50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = wt_int), adjust = 1, lwd = 2, geom = "line", position = "identity") +
  facet_grid(sex ~ .) +
  theme_ipsum() +
  labs(x = "kcal/day",
       color = "",
       title = "Distribution kcal/day") +
  theme_ipsum(base_size = 15, axis_title_size = 15) +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) 

# change in intake

obese_df %>% 
  mutate(sex = ifelse(sex_label == "female", "Obese Females", "Obese Males")) %>% 
  mutate(bmi_class = factor(bmi_class, levels = c("underweight","normal", "overweight", "obese", "morb obese"))) %>% 
  dplyr::select(calRed, sex, wt_int, bmi_class) %>% 
  ggplot(., aes(x = calRed, fill = bmi_class)) +
  stat_density(aes(weight = wt_int), adjust = 3, lwd = 2, geom = "line", position = "identity", color = "#0000FF") +
  scale_x_continuous(labels = scales::percent) +
  facet_grid(sex ~ .) +
  labs(x = "kcal/day",
       color = "",
       title = "Distribution of Difference in kcal/day: Obese",
       subtitle = "As a % of baseline intake") +
  theme_ipsum(base_size = 15, axis_title_size = 15)


full$bmi_class <- factor(full$bmi_class, levels = c("underweight", "normal", "overweight", "obese", "morb obese"))
full$final_BMI_class <- factor(full$final_BMI_class, levels = c("underweight", "normal", "overweight", "obese", "morb obese"))

rbind(
  full %>% 
  count(final_BMI_class, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100,
         type = "50% reduction") %>% 
    rename(BMI = final_BMI_class),
  full %>% 
    count(bmi_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Now") %>% 
    rename(BMI = bmi_class))

rbind(
  full %>% 
  count(final_BMI_class, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100,
         type = "Simulated 50% Reduction") %>% 
    rename(BMI = final_BMI_class),
  full %>% 
    count(bmi_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "2019") %>% 
    rename(BMI = bmi_class))  %>% 
ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population") +
  theme_ipsum(base_size = 15, axis_title_size = 15) +
  scale_fill_manual(values=c("#0000FF", "#F6A4B7")) 
  

rbind(
  full %>% 
  dplyr::select(bmi_final, wt_int) %>% 
  mutate(type = "50% reduction") %>% 
  rename(BMI = bmi_final),
  full %>% 
    dplyr::select(bmival, wt_int) %>% 
    mutate(type = "Now") %>% 
    rename(BMI = bmival))  %>% 
ggplot(., aes(x = BMI, color = type)) + 
  geom_density(aes(weight = wt_int), adjust = 2) +
  theme_ipsum() +
  labs(color = "", title = "BMI Distribution")

# difference in density intake distribution


plot1 <- obese_df %>% 
  mutate(sex = ifelse(sex_label == "female", "Obese Females", "Obese Males")) %>% 
  dplyr::select(baseIntake, intake_final, sex, wt_int, bmi_class) %>% 
  melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
  mutate(label = ifelse(variable == "baseIntake", "2019", "Simulated 50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = wt_int), adjust = 1, lwd = 2, geom = "line", position = "identity") +
  facet_grid(sex ~ .) +
  theme_ipsum() +
  labs(x = "kcal/day",
       color = "",
       title = "Distribution kcal/day: Obese") +
  theme_ipsum(base_size = 15, axis_title_size = 15) +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")
                        


density_diff <- rbind(
  obese_df %>% 
    mutate(sex = ifelse(sex_label == "female", "Obese Females", "Obese Males")) %>% 
    filter(sex == "Obese Females") %>% 
    dplyr::select(baseIntake, intake_final, sex, wt_int, bmi_class) %>% 
    melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
    mutate(label = ifelse(variable == "baseIntake", "2019", "Simulated 50% Reduction")) %>% 
    group_by(label) %>% 
    # calculate densities for each group over same range; store in list column
    summarise(d = list(density(value, from = min(.$value), to = max(.$value)))) %>% 
    # make a new data.frame from two density objects
    do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                  y = .$d[[2]]$y - .$d[[1]]$y)) %>%    # and subtract the y values
    mutate(sex = "Obese Females"),
  obese_df %>% 
    mutate(sex = ifelse(sex_label == "female", "Obese Females", "Obese Males")) %>% 
    filter(sex == "Obese Males") %>% 
    dplyr::select(baseIntake, intake_final, sex, wt_int, bmi_class) %>% 
    melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
    mutate(label = ifelse(variable == "baseIntake", "2019", "Simulated 50% Reduction")) %>% 
    group_by(label) %>% 
    # calculate densities for each group over same range; store in list column
    summarise(d = list(density(value, from = min(.$value), to = max(.$value)))) %>% 
    # make a new data.frame from two density objects
    do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                  y = .$d[[2]]$y - .$d[[1]]$y)) %>%    # and subtract the y values
    mutate(sex = "Obese Males"))

options(scipen=100)

plot2 <- ggplot(density_diff, aes(x, y)) + 
  geom_line(color = "#FF6E47", size = 3) +
  geom_hline(yintercept = 0) +
  facet_grid(sex ~ . ) +
  labs(x = "kcal/day",
       color = "",
       title = "Difference in kcal/day: Obese \n (Simulated - 2019)",
       y = "Density difference") +
  theme_ipsum(base_size = 15, axis_title_size = 15) 

plot2

grid.arrange(plot1, plot2, ncol=2)


# sankey men

tab <- full %>% 
  filter(sex_label == "male") %>% 
  count(bmi_class, final_BMI_class, wt = wt_int) %>% 
  mutate(freq = round(n/sum(n)*100,2))

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=tab$bmi_class, 
  target=tab$final_BMI_class, 
  value=tab$freq
)

links$source <- paste0(links$source, '_1')
links$target <- paste0(links$target, '_2')


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)


# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

nodes$name <- sub('_[1-2]$', '', nodes$name)


# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE,
                   fontSize = 20,
                   nodeWidth = 30,
                   width= 900, height=600)


p <- htmlwidgets::prependContent(p, htmltools::tags$h1("Men (%)"))



htmlwidgets::onRender(p, '
  function(el) { 
    var nodeWidth = this.sankey.nodeWidth();
    var links = this.sankey.links();
        
    links.forEach((d, i) => {
      var startX = d.source.x + nodeWidth;
      var endX = d.target.x;
      
      var startY = d.source.y + d.sy + d.dy / 2;
      var endY = d.target.y + d.ty + d.dy / 2;
      
      d3.select(el).select("svg g")
        .append("text")
        .attr("text-anchor", "middle")
        .attr("alignment-baseline", "middle")
        .attr("x", startX + ((endX - startX) / 2))
        .attr("y", startY + ((endY - startY) / 2))
        .text(d.value);
    })
  }
')

# sankey women

tab <- full %>% 
  filter(sex_label == "female") %>% 
  count(bmi_class, final_BMI_class, wt = wt_int) %>% 
  mutate(freq = round(n/sum(n)*100,2))

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=tab$bmi_class, 
  target=tab$final_BMI_class, 
  value=tab$freq
)

links$source <- paste0(links$source, '_1')
links$target <- paste0(links$target, '_2')


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)


# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

nodes$name <- sub('_[1-2]$', '', nodes$name)


# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE,
                   fontSize = 20,
                   nodeWidth = 30,
                   width= 900, height=600)


p <- htmlwidgets::prependContent(p, htmltools::tags$h1("Women (%)"))



htmlwidgets::onRender(p, '
  function(el) { 
    var nodeWidth = this.sankey.nodeWidth();
    var links = this.sankey.links();
        
    links.forEach((d, i) => {
      var startX = d.source.x + nodeWidth;
      var endX = d.target.x;
      
      var startY = d.source.y + d.sy + d.dy / 2;
      var endY = d.target.y + d.ty + d.dy / 2;
      
      d3.select(el).select("svg g")
        .append("text")
        .attr("text-anchor", "middle")
        .attr("alignment-baseline", "middle")
        .attr("x", startX + ((endX - startX) / 2))
        .attr("y", startY + ((endY - startY) / 2))
        .text(d.value);
    })
  }
')

# Compare with 1991 distribution

df_1991 <- read.csv("/Users/elenamariani/Documents/working/weight_loss_modelling/processed_data/hse1991_clean_no_outliers.csv")

df_all <- rbind(df_1991  %>% 
                dplyr::select(bmivalid)  %>% 
                mutate(year = "1991-92",
                      wt_int = 1)  %>% 
                rename(bmi = bmivalid),
               full  %>% 
                dplyr::select(bmi_final, wt_int)  %>%
                mutate(year = "Simulated 50% Reduction")  %>% 
                rename(bmi = bmi_final))  %>% 
mutate(bmi_class = case_when(bmi <= 18.5 ~ "underweight",
                               bmi > 18.5 & bmi < 25 ~ "normal",
                               bmi >= 25 & bmi < 30 ~ "overweight",
                             bmi >= 30 & bmi < 40 ~ "obese",
                             bmi >= 40 ~ "morb obese",
                               TRUE ~ "NA"))

df_all$bmi_class <- factor(df_all$bmi_class, levels = c("underweight", "normal", "overweight", "obese", "morb obese"))

plotA <- df_all %>% 
  filter(bmi_class %in% c("obese")) %>% 
  ggplot(., aes(x = bmi, color = year)) + 
geom_density(aes(weight = wt_int), size = 2, adjust = 2, alpha = 0.5) +
facet_grid(bmi_class ~ . ) +
theme_ipsum() +
labs(title = "Obese: BMI Distribution",
     color = "",
     x = "BMI")+
  xlim(20,60) +
  theme_ipsum(base_size = 15, axis_title_size = 15) +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")

plotA


density_diff <- df_all %>% 
    group_by(year) %>% 
    # calculate densities for each group over same range; store in list column
    summarise(d = list(density(bmi, from = min(.$bmi), to = max(.$bmi)))) %>% 
    # make a new data.frame from two density objects
    do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                  y = .$d[[2]]$y - .$d[[1]]$y))    # and subtract the y values

options(scipen=100)

plotB <- ggplot(density_diff, aes(x, y)) + 
  geom_line(color = "#FF6E47", size = 3) +
  geom_hline(yintercept = 0) +
  labs(x = "BMI",
       color = "",
       title = "Difference in BMI Distribution (Simulated - 1991-92)",
       y = "Density difference") +
  ylim(-0.2, 0.2) +
  theme_ipsum(base_size = 15, axis_title_size = 15) 

plotB

grid.arrange(plotA, plotB, ncol=2)
