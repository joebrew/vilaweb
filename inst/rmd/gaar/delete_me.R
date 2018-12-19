tweets <- bind_rows(out_list)
people <- bind_rows(guys_list)
people_tls <- bind_rows(tl_list)

# Compare vaga de fam with comandos separatistas
x <- people %>%
  filter(source != 'Presos polítics') %>%
  group_by(source, lang) %>%
  tally %>%
  group_by(source) %>%
  mutate(p = n / sum(n) * 100) %>%
  arrange(desc(p))
  summarise(n = n(),
            fake = length(which(followers_count <= 3)),
            fake2 = length(which(friends_count <= 3)),
            fake3 = length(which(profile_image_url == 'http://abs.twimg.com/sticky/default_profile_images/default_profile_normal.png'))) %>%
  ungroup %>%
  mutate(p = fake / n * 100,
         p2 = fake2 / n * 100,
         p3 = fake3 / n * 100)
x

ggplot(data = people %>%
         # filter(date >= '2018-01-01') %>%
         filter(source %in% c('Comandos separatistas', 'Vaga de fam')),
       aes(x = account_created_at)) +
  geom_density(aes(group = source,
                   fill = source),
               alpha = 0.6)

new_tl <- improve_timeline(people_tls)

ggplot(data = new_tl %>%
         filter(date >= '2018-01-01') %>%
         filter(source %in% c('Comandos separatistas', 'Vaga de fam')),
       aes(x = time_only,
           y = date,
           color = source)) +
  geom_point(size = 0.1)

ggplot(data = new_tl %>%
         filter(date >= '2018-01-01') %>%
         filter(source %in% c('Comandos separatistas', 'Vaga de fam')),
       aes(x = time_only)) +
  geom_density(aes(fill = source,
                   group = source),
               alpha = 0.6)

ggplot(data = new_tl %>%
         filter(date >= '2018-01-01') %>%
         filter(source %in% c('Comandos separatistas', 'Vaga de fam')) %>%
         group_by(hour, source) %>%
         tally %>%
         ungroup %>%
         group_by(source) %>%
         mutate(p = n / sum(n) * 100),
       aes(x = hour,
           y = p)) +
  geom_bar(stat = 'identity',
           aes(fill = source,
                   group = source),
               alpha = 0.6)
