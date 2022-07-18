library(lubridate)

reduction_5 <- reduction_50

mean(reduction_5$day_final)

reduction_5 %>% 
  group_by(final_BMI_class) %>% 
  summarise(mean = mean(day_final))

paste0(days(round(median(reduction_5$day_final),0)) %/% years(1) , " years and ", days(round(median(reduction_5$day_final),0)) %% years(1) %/% months(1), " months")

paste0(days(round(max(reduction_5$day_final),0)) %/% years(1) , " years and ", days(round(max(reduction_5$day_final),0)) %% years(1) %/% months(1), " months")

paste0(days(round(min(reduction_5$day_final),0)) %/% years(1) , " years and ", days(round(min(reduction_5$day_final),0)) %% years(1) %/% months(1), " months")

ggplot(reduction_5, aes(x = BMI_est, y = day_final)) + geom_point()

ggplot(reduction_5, aes(x = day_final)) + geom_density()       

median(reduction_5$baseIntake)

median(reduction_5$newIntake)
(mean(reduction_5$newIntake) - mean(reduction_5$baseIntake))/mean(reduction_5$baseIntake)
