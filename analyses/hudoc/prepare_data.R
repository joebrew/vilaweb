# Data exported from:https://hudoc.echr.coe.int/eng#{%22sort%22:[%22kpdate%20Ascending%22],%22respondent%22:[%22ESP%22],%22documentcollectionid2%22:[%22GRANDCHAMBER%22,%22CHAMBER%22]}
library(tidyverse)
echr <- read_csv('data/hudoc_oldest.csv')
echr2 <- read_csv('data/hudoc_newest.csv')
echr <- bind_rows(echr, echr2)
echr <- echr %>% distinct(`Application Number`, .keep_all = T)
# echr <- echr %>% filter(!duplicated(`Application Number`))
echr$Date <- as.Date(echr$Date, format = '%d/%m/%Y')
echr$year <- as.numeric(format(echr$Date, '%Y'))

echr %>%
  group_by(year) %>%
  tally %>%
  ggplot(aes(x = year, y = n)) + geom_line()
