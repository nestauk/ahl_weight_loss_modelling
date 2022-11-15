rm(list = ls())

library(Hmisc)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(reshape2)
library(survey)
library(networkD3)
library(here)
library(grid)
library(gridExtra)

options(scipen=100)



# read 2019 data

df_2019 <- read_csv(here("outputs/data/hse_2019_clean.csv"))%>% 
  mutate(sex = ifelse(sex == 2 ,"female", "male"))

# mean weight change
w_change <- read.csv(here("outputs/reports/weight_change_table_mean.csv")) %>% 
  dplyr::select(bmi_class, female, male) %>% 
  melt(., id.vars = "bmi_class")

# merge data to obtain weight target
dat <- merge(df_2019, w_change, by.x = c("bmi_class", "sex"), by.y = c("bmi_class", "variable")) %>% 
  mutate(target = weight*(1 - value/100))

# obese only

obese_df <- read_csv(here("outputs/data/obese_all_years.csv")) %>% 
  filter(year == 3) %>%
  merge(., dat, by = "id") %>% 
  mutate(bmi_final = target/(height/100)^2) %>% 
  mutate(bmi_class_final = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                     bmi_final >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA")) %>% 
  mutate(intake_start = pal*rmr,
         intake_end = (pal*rmr) + ei)

# overweight only

over_df <- read_csv(here("outputs/data/over_3_year.csv"))%>%
  merge(., dat, by = "id") %>% 
  mutate(bmi_final = target/(height/100)^2) %>% 
  mutate(bmi_class_final = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                     bmi_final >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA")) %>% 
  mutate(intake_start = pal*rmr,
         intake_end = (pal*rmr) + ei)

# morbidly obese only

morb_df <- read_csv(here("outputs/data/morb_3_year.csv"))%>%
  merge(., dat, by = "id") %>% 
  mutate(bmi_final = target/(height/100)^2) %>% 
  mutate(bmi_class_final = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                     bmi_final >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA")) %>% 
  mutate(intake_start = pal*rmr,
         intake_end = (pal*rmr) + ei)

# get rest of the population

full <- rbind(read_csv(here("outputs/data/hse_2019_clean.csv")) %>% 
                filter(bmi_class %in% c("underweight", "normal")) %>% 
                mutate(days = 365*3,
                       ei = 0,
                       year = 3) %>% 
                dplyr::select(id, days, ei, year),
              read_csv(here("outputs/data/obese_all_years.csv")),
              read_csv(here("outputs/data/over_3_year.csv")),
              read_csv(here("outputs/data/morb_3_year.csv"))) %>% 
  filter(year == 3) %>%
  merge(., dat, by = "id") %>% 
  mutate(bmi_final = ifelse(bmi_class %in% c("underweight", "normal"), weight/(height/100)^2, target/(height/100)^2)) %>% 
  mutate(bmi_class_final = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 & bmi_final < 40 ~ "obese",
                                     bmi_final >= 40 ~ "morbidly obese",
                                     TRUE ~ "NA")) %>% 
  mutate(intake_start = pal*rmr,
         intake_end = (pal*rmr) + ei)

full$bmi_class <- factor(full$bmi_class, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))
full$bmi_class_final <- factor(full$bmi_class_final, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))


# descriptive stats - all
rbind(full %>% dplyr::select(bmi_class, sex, intake_start, wt_int) %>% mutate(type = "base") %>% rename(intake = intake_start),
      full %>% dplyr::select(bmi_class, sex, intake_end, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_end)) %>% 
  group_by(sex, type) %>% 
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))

# descriptive stats - excess weight
rbind(full %>% dplyr::select(bmi_class, sex, intake_start, wt_int) %>% mutate(type = "base") %>% rename(intake = intake_start),
      full %>% dplyr::select(bmi_class, sex, intake_end, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_end)) %>% 
  filter(bmi_class %in% c("overweight", "obese", "morbidly obese")) %>% 
  group_by(sex, type) %>% 
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))

# descriptive stats - obese
rbind(obese_df %>% dplyr::select(bmi_class, sex, intake_start, wt_int) %>% mutate(type = "base") %>% rename(intake = intake_start),
      obese_df %>% dplyr::select(bmi_class, sex, intake_end, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_end)) %>% 
  group_by(bmi_class, sex, type) %>% 
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., bmi_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))

# descriptive stats - overweight
rbind(over_df %>% dplyr::select(bmi_class, sex, intake_start, wt_int) %>% mutate(type = "base") %>% rename(intake = intake_start),
      over_df %>% dplyr::select(bmi_class, sex, intake_end, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_end)) %>% 
  group_by(bmi_class, sex, type) %>% 
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., bmi_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))

# descriptive stats - morbidly
rbind(morb_df %>% dplyr::select(bmi_class, sex, intake_start, wt_int) %>% mutate(type = "base") %>% rename(intake = intake_start),
      morb_df %>% dplyr::select(bmi_class, sex, intake_end, wt_int) %>% mutate(type = "final") %>% rename(intake = intake_end)) %>% 
  group_by(bmi_class, sex, type) %>% 
  summarise(intakeM = round(wtd.mean(intake, weight = wt_int),0)) %>% 
  dcast(., bmi_class + sex ~ type) %>% 
  arrange(sex) %>% 
  mutate(diff = final - base,
         perc = round(diff/base*100,1))

# descriptives for the difference

full %>% 
  mutate(diff = intake_end - intake_start) %>% 
  group_by(bmi_class, sex) %>% 
  summarise(std = round(sqrt(wtd.var(diff, weight = wt_int)),1),
            min = min(diff),
            max = max(diff),
            perc25 = wtd.quantile(diff, probs = 0.25, weight = wt_int),
            perc75 = wtd.quantile(diff, probs = 0.75, weight = wt_int),
            iqr = perc75 - perc25)

full %>% 
  mutate(calRed = ei/intake_start) %>% 
  group_by(bmi_class, sex) %>% 
  summarise(std = round(sqrt(wtd.var(calRed, weight = wt_int)),1),
            min = min(calRed),
            max = max(calRed),
            perc25 = wtd.quantile(calRed, probs = 0.25, weight = wt_int),
            perc75 = wtd.quantile(calRed, probs = 0.75, weight = wt_int),
            iqr = perc75 - perc25)

# confidence interval

dclus2 <- svydesign(id=~id, weights = ~wt_int, data=full)

funDiff_byBMI <- function(bmi_class_c, sex_c){
    sub <- subset(dclus2, sex == sex_c & bmi_class == bmi_class_c)
    tt<-svyttest(I(intake_end - intake_start)~0, sub)
    return(data.frame(sex = sex_c, bmi_class = bmi_class_c, confint(tt, level=0.95)))
}

funDiff_excess <- function(sex_c){
  sub <- subset(dclus2, sex == sex_c & bmi_class %in% c("overweight", "obese", "morbidly obese"))
  tt<-svyttest(I(intake_end - intake_start)~0, sub)
  return(data.frame(sex = sex_c, confint(tt, level=0.95)))
}

funDiff_all <- function(sex_c){
  sub <- subset(dclus2, sex == sex_c)
  tt<-svyttest(I(intake_end - intake_start)~0, sub)
  return(data.frame(sex = sex_c, confint(tt, level=0.95)))
}

listBMI <- expand.grid(sex_c = unique(full$sex), bmi_class_c = c("underweight","normal","overweight", "obese", "morbidly obese"))

pmap_dfr(listBMI, funDiff_byBMI)  %>% arrange(sex)

listSex <- expand.grid(sex_c = unique(full$sex))

pmap_dfr(listSex, funDiff_all)  %>% arrange(sex)

pmap_dfr(listSex, funDiff_excess)  %>% arrange(sex)


# Intake distribution

full %>% 
  filter(bmi_class %in% c("overweight", "obese", "morbidly obese")) %>% 
  dplyr::select(intake_start, intake_end, sex, wt_int, bmi_class) %>% 
  melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
  mutate(label = ifelse(variable == "intake_start", "2019", "Simulated 50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = wt_int), adjust = 1, lwd = 2, geom = "line", position = "identity") +
  facet_grid(bmi_class  ~ sex) +
  labs(x = "kcal/day",
       color = "",
       title = "Distribution kcal/day") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")

ggsave(here("outputs/figures/intake_distribution.png"), width = 10, bg='#ffffff')
dev.off()

# change in intake

full %>% 
  filter(bmi_class %in% c("overweight", "obese", "morbidly obese")) %>% 
  mutate(calRed = ei/intake_start) %>% 
  dplyr::select(calRed, sex, wt_int, bmi_class) %>% 
  ggplot(., aes(x = calRed, fill = bmi_class)) +
  stat_density(aes(weight = wt_int), adjust = 3, lwd = 2, geom = "line", position = "identity", color = "#0000FF") +
  scale_x_continuous(labels = scales::percent) +
  facet_grid(bmi_class ~ sex) +
  labs(x = "kcal/day",
       color = "",
       title = "Distribution of Difference in kcal/day",
       subtitle = "As a % of baseline intake") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta")

ggsave(here("outputs/figures/intake_change_distribution.png"), width = 10, bg='#ffffff')
dev.off()

rbind(
  full %>% 
  count(bmi_class_final, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100,
         type = "50% reduction") %>% 
    rename(BMI = bmi_class_final),
  full %>% 
    count(bmi_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Now") %>% 
    rename(BMI = bmi_class))

rbind(
  full %>% 
  count(bmi_class_final, wt = wt_int) %>% 
  mutate(freq = n/sum(n)*100,
         type = "Simulated 50% Reduction") %>% 
    rename(BMI = bmi_class_final),
  full %>% 
    count(bmi_class, wt = wt_int) %>% 
    mutate(freq = n/sum(n)*100,
           type = "2019") %>% 
    rename(BMI = bmi_class))  %>% 
  mutate(BMI = factor(BMI, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))) %>% 
ggplot(., aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", 
       title = "BMI Categories Distribution", 
       y = "Frequency",
       subtitle = "Population") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_fill_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")
  
ggsave(here("outputs/figures/bmi_distribution_50reduction.png"), width = 10, bg='#ffffff')
dev.off()

rbind(
  full %>% 
  dplyr::select(bmi_final, wt_int) %>% 
  mutate(type = "50% reduction") %>% 
  rename(BMI = bmi_final),
  full %>% 
    dplyr::select(bmi, wt_int) %>% 
    mutate(type = "Now") %>% 
    rename(BMI = bmi))  %>% 
ggplot(., aes(x = BMI, color = type)) + 
  geom_density(aes(weight = wt_int), adjust = 2) +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  labs(color = "", title = "BMI Distribution") +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")

# difference in density intake distribution


plot1 <- obese_df %>% 
  dplyr::select(intake_start, intake_end, sex, wt_int, bmi_class) %>% 
  melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
  mutate(label = ifelse(variable == "intake_start", "2019", "Simulated 50% Reduction")) %>% 
  ggplot(., aes(x = value, group = label, color = label)) +
  stat_density(aes(weight = wt_int), adjust = 1, lwd = 2, geom = "line", position = "identity") +
  facet_grid(sex ~ .) +
  theme_ipsum() +
  labs(x = "kcal/day",
       color = "",
       title = "Distribution kcal/day: Obese") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
  scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")
                        


density_diff <- rbind(
  obese_df %>% 
    mutate(sex = ifelse(sex == "female", "Obese Females", "obese Males")) %>% 
    filter(sex == "Obese Females") %>% 
    dplyr::select(intake_start, intake_end, sex, wt_int, bmi_class) %>% 
    melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
    mutate(label = ifelse(variable == "intake_start", "2019", "Simulated 50% Reduction")) %>% 
    group_by(label) %>% 
    # calculate densities for each group over same range; store in list column
    summarise(d = list(density(value, from = min(.$value), to = max(.$value)))) %>% 
    # make a new data.frame from two density objects
    do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                  y = .$d[[2]]$y - .$d[[1]]$y)) %>%    # and subtract the y values
    mutate(sex = "Obese Females"),
  obese_df %>% 
    mutate(sex = ifelse(sex == "female", "Obese Females", "Obese Males")) %>% 
    filter(sex == "Obese Males") %>% 
    dplyr::select(intake_start, intake_end, sex, wt_int, bmi_class) %>% 
    melt(., id.vars = c("sex", "wt_int", "bmi_class")) %>% 
    mutate(label = ifelse(variable == "intake_start", "2019", "Simulated 50% Reduction")) %>% 
    group_by(label) %>% 
    # calculate densities for each group over same range; store in list column
    summarise(d = list(density(value, from = min(.$value), to = max(.$value)))) %>% 
    # make a new data.frame from two density objects
    do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                  y = .$d[[2]]$y - .$d[[1]]$y)) %>%    # and subtract the y values
    mutate(sex = "Obese Males"))



plot2 <- ggplot(density_diff, aes(x, y)) + 
  geom_line(color = "#FF6E47", size = 3) +
  geom_hline(yintercept = 0) +
  facet_grid(sex ~ . ) +
  labs(x = "kcal/day",
       color = "",
       title = "Difference in kcal/day: Obese \n (Simulated - 2019)",
       y = "Density difference") +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta")
  
png(here("outputs/figures/intake_all.png"), width = 1000)
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# sankey men

tab <- full %>% 
  filter(sex == "male") %>% 
  count(bmi_class, bmi_class_final, wt = wt_int) %>% 
  mutate(freq = round(n/sum(n)*100,2))

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=tab$bmi_class, 
  target=tab$bmi_class_final, 
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
  filter(sex == "female") %>% 
  count(bmi_class, bmi_class_final, wt = wt_int) %>% 
  mutate(freq = round(n/sum(n)*100,2))

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=tab$bmi_class, 
  target=tab$bmi_class_final, 
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

df_1991 <- read.csv(here("outputs/data/hse_1991_clean.csv"))

df_comparison <- rbind(df_1991 %>% 
                         dplyr::select(wt_int, bmi_class, bmi) %>% 
                         rename(bmi_class_final = bmi_class,
                                bmi_final = bmi) %>% 
                         mutate(year = "1991-92"),
                       full %>% 
                         dplyr::select(wt_int, bmi_class_final, bmi_final) %>% 
                         mutate(year = "50% Simulated Reduction"))

df_comparison$bmi_class <- factor(df_comparison$bmi_class_final, levels = c("underweight", "normal", "overweight", "obese", "morbidly obese"))

plotA <- df_comparison %>% 
  filter(bmi_class %in% c("overweight", "obese", "morbidly obese")) %>% 
  ggplot(., aes(x = bmi_final, color = year)) + 
geom_density(aes(weight = wt_int), size = 2, adjust = 2, alpha = 0.5) +
facet_grid(bmi_class ~ . ) +
theme_ipsum() +
labs(title = "BMI Distribution",
     color = "",
     x = "BMI")+
  xlim(20,60) +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta") +
scale_color_manual(values=c("#0000FF", "#F6A4B7")) +
  theme(legend.position = "top")

ggsave(here("outputs/figures/bmi_difference_1991.png"), width = 10, bg='#ffffff')
dev.off()


density_diff <- df_comparison %>% 
    group_by(year) %>% 
    # calculate densities for each group over same range; store in list column
    summarise(d = list(density(bmi_final, from = min(.$bmi_final), to = max(.$bmi_final)))) %>% 
    # make a new data.frame from two density objects
    do(data.frame(x = .$d[[1]]$x,    # grab one set of x values (which are the same)
                  y = .$d[[2]]$y - .$d[[1]]$y))    # and subtract the y values


plotB <- ggplot(density_diff, aes(x, y)) + 
  geom_line(color = "#FF6E47", size = 3) +
  geom_hline(yintercept = 0) +
  labs(x = "BMI",
       color = "",
       title = "Difference in BMI Distribution (Simulated - 1991-92)",
       y = "Density difference") +
  ylim(-0.2, 0.2) +
  theme_ipsum(base_size = 15, axis_title_size = 15, base_family="Averta")



png(here("outputs/figures/bmi_difference_1991.png"), width = 1000)
grid.arrange(plotA, plotB, ncol=2)
dev.off()
