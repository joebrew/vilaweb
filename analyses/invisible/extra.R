

# Appendix

What follows are some more details on 2017 vs. 2018 twitter attentio

## Tweets from sovereigntists referencing the prisoners and exilees

The silencing effect is not universal It appears to occur mostly from the imprisoned's political adversaries. Allies of the imprisoned - other sovereigntists and independentists - largely increased the frequency with which they tweeted about Oriol Junqueras after his imprisonment.

```{r, fig.height = 8}
make_plot(language = 'en',
who = 'junqueras',
people = unique(pd$person[pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Oriol Junqueras',
subtitle = 'Before and after exile')
```

The below shows the same data for Puigdemont. With few exceptions, sovereigntist politicians tweeted about Puigdemont _more_ after his exile than before.

```{r}
make_plot(language = 'en',
who = 'puigdemont',
people = unique(pd$person[pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Carles Puigdemont',
subtitle = 'Before and after exile')
```

The below shows the same data for Carme Forcadell. Like other prisoners and exilees, most sovereigntists tweeted about her more following her imprisonment than prior to it.

```{r}
make_plot(language = 'en',
who = 'forcadell',
people = unique(pd$person[pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Carme Forcadell',
subtitle = 'Before and after imprisonment')
```

The below shows the same data for Toni Comín. He is lesser-known than the other politicians, but still saw a _general_ increase in tweets from political allies in 2018 relative to 2017.

```{r}
make_plot(language = 'en',
who = 'comin',
people = unique(pd$person[pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Toni Comín',
subtitle = 'Before and after exile')
```

## Tweets from unionists referencing the prisoners and exilees

```{r}
make_plot(language = 'en',
who = 'junqueras',
people = unique(pd$person[!pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Oriol Junqueras',
subtitle = 'Before and after imprisonment')
```


```{r}
make_plot(language = 'en',
who = 'puigdemont',
people = unique(pd$person[!pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Carles Puigdemont',
subtitle = 'Before and after exile')
```

```{r}
make_plot(language = 'en',
who = 'forcadell',
people = unique(pd$person[!pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Carme Forcadell',
subtitle = 'Before and after imprisonment')
```

```{r}
make_plot(language = 'en',
who = 'comin',
people = unique(pd$person[!pd$eix_indepe_unio %in% c('indepe', 'sobiranista')])) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Toni Comín',
subtitle = 'Before and after exile')
```

## By party

```{r}
party_data <- 
pd %>%
group_by(person = party, year) %>%
summarise(puigdemont = sum(puigdemont),
junqueras = sum(junqueras),
forcadell = sum(forcadell),
comin = sum(comin))
```

Junqueras

```{r}
make_plot(data = party_data,
people = sort(unique(pd$party)),
language = 'en',
who = 'junqueras',
add_at = FALSE) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Oriol Junqueras',
subtitle = 'Before and after imprisonment')
```

Puigdemont

```{r}
make_plot(data = party_data,
people = sort(unique(pd$party)),
language = 'en',
who = 'puigdemont',
add_at = FALSE) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Carles Puigdemont',
subtitle = 'Before and after exile')
```

Forcadell

```{r}
make_plot(data = party_data,
people = sort(unique(pd$party)),
language = 'en',
who = 'forcadell',
add_at = FALSE) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Carme Forcadell',
subtitle = 'Before and after imprisonment')
```

Comín

```{r}
make_plot(data = party_data,
people = sort(unique(pd$party)),
language = 'en',
who = 'comin',
add_at = FALSE) +
labs(x = '',
y = 'Tuits',
title = 'Twitter references to Toni Comín',
subtitle = 'Before and after exile')
```


