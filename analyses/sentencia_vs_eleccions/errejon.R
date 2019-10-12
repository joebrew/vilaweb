erre <- get_twitter_data_from_database(people = 'ierrejon')
erre$catalunya <- grepl('cataluña|catala|cataluny', tolower(erre$tweet))
erre$auto <- grepl('autodeterminación', tolower(erre$tweet))
erre$asturias <- grepl('asturias', tolower(erre$tweet))

erre$month <- as.Date(cut(erre$date, 'month'))
erre$year <- as.Date(cut(erre$date, 'year'))

pd <- erre %>%
  group_by(month, catalunya) %>%
  tally %>%
  group_by(month) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(catalunya)
left <- expand.grid(month = seq(min(erre$month),
                                max(erre$month),
                                by = 'month'))

pd <- left_join(left, pd)
pd$p[is.na(pd$p)] <- 0
ggplot(data = pd,
       aes(x = month,
           y = p)) +
  geom_bar(stat = 'identity')

recent <- erre %>% filter(catalunya)
View(recent)

recent <- erre %>% filter(asturias)
View(recent)
