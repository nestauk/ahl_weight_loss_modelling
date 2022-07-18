# Library
library(networkD3)
library(dplyr)
library(tidyverse)
library(htmlwidgets)
library(scales)


under <- read.csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Data\\calorie_deficit_scenarios_w_imd.csv", sep = ",", fileEncoding="UTF-8-BOM") %>% 
  filter(BMI_est > 15 & BMI_est <= 18.5) %>% 
  mutate(final_BMI_class = BMI_class,
         bmi_final = BMI_est,
         weight_final = Wt_est)


model <- read_csv("C:\\Users\\Elena.Mariani\\Documents\\Projects\\ahl_weight_loss_modelling\\Output\\hse_full_weight_loss.csv") %>% 
  mutate(BMI_class = case_when(BMI_est <= 18.5 ~ "underweight",
                               BMI_est > 18.5 & BMI_est < 25 ~ "normal",
                               BMI_est >= 25 & BMI_est < 30 ~ "overweight",
                               BMI_est >= 30 ~ "obese",
                               TRUE ~ "NA")) %>% 
  mutate(final_BMI_class = case_when(bmi_final <= 18.5 ~ "underweight",
                                     bmi_final > 18.5 & bmi_final < 25 ~ "normal",
                                     bmi_final >= 25 & bmi_final < 30 ~ "overweight",
                                     bmi_final >= 30 ~ "obese",
                                     TRUE ~ "NA")) %>% 
  dplyr::select(names(under))


full <- rbind(model, under)

full$BMI_class <- factor(full$BMI_class, levels = c("underweight", "normal", "overweight", "obese"))
full$final_BMI_class <- factor(full$final_BMI_class, levels = c("underweight", "normal", "overweight", "obese"))

final <- rbind(
  full %>% 
  count(final_BMI_class, wt = sample_weight) %>% 
  mutate(freq = n/sum(n)*100,
         type = "50% reduction") %>% 
    rename(BMI = final_BMI_class),
  full %>% 
    count(BMI_class, wt = sample_weight) %>% 
    mutate(freq = n/sum(n)*100,
           type = "Now") %>% 
    rename(BMI = BMI_class)) 


ggplot(final, aes(y = freq, x = BMI, fill = type)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_ipsum() +
  labs(fill = "", title = "BMI Categories Distribution")

tab <- full %>% 
  filter(Sex == "male") %>% 
  count(BMI_class, final_BMI_class, wt = sample_weight) %>% 
  mutate(freq = round(n/sum(n)*100,2))

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=tab$BMI_class, 
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
p

p <- htmlwidgets::prependContent(p, htmltools::tags$h1("Men (%)"))

p

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
