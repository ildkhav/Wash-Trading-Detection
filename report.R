library(igraph)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(anytime)

# load the data
df <- fromJSON('https://raw.githubusercontent.com/ildkhav/Wash-Trading-Detection/main/ALPINEBTC.json')
as_tibble(df)

# start, end time
anytime(as.numeric(substr(min(df$T), 1, 10)), tz = 'UTC')
anytime(as.numeric(substr(max(df$T), 1, 10)), tz = 'UTC')

# make numeric
df$p <- as.double(df$p)
df$q <- as.double(df$q)

# quantity distribution
ggplot(data = df) +
  ggtitle('Quantity distribution') +
  xlab('quantity') +
  geom_histogram(mapping = aes(q), binwidth = 5)

# all quantities sum
sum(df$q)

# all trades
ggplot(data = df, mapping = aes(anytime(as.numeric(substr(T, 1, 10))), q)) +
  ggtitle('Trades') +
  xlab('time') +
  ylab('quantity') +
  geom_point()

# time gap distribution
df %>%
  mutate(dT = T- lag(T)) %>%
  ggplot() +
  ggtitle('Trade time gap distribution') +
  xlab('ms') +
  geom_histogram(mapping = aes(dT)) +
  scale_x_continuous(labels = scales::comma, trans = 'log10')

# filter out market maker and check
orig_df <- df
df <- df %>%
  filter(df$m == FALSE)
nrow(df)
df <- na.omit(df)
nrow(df)

# a trades graph example
data.frame(a = c('A1', 'A1'), b = c('B10', 'B15')) %>%
  graph_from_data_frame() %>%
  plot(vertex.size = 1)

# make a graph
g <- df %>% 
  select(a, b, t) %>%
  graph_from_data_frame()

c <- components(g, mode = "weak")
c$no

grps <- igraph::groups(c)

# members distribution
ggplot() +
  ggtitle('Cluster size distribution') +
  xlab('cluster size') +
  geom_histogram(aes(c[["csize"]]), binwidth = 2)

# sequential vertices list
l <- lapply(grps, function(i) {
  if(all(abs(diff(sort(unique(as.numeric(i))))) == 1)) {
    return(c[['membership']][[i[1]]])
  } else {
    return(NA)
  }
})

# a data frame from the graph
xdf <- igraph::as_data_frame(g)
as_tibble(xdf)

# assign membership
msh <- sapply(xdf$from, function(x) {
  c[["membership"]][[x]]
})
xdf$membership <- msh
as_tibble(xdf)

# filter membership of interest
xdf <- xdf %>%
  filter(membership %in% l)
as_tibble(xdf)

# amount of trades distribution
xdf %>%
  group_by(membership) %>%
  mutate(n = n()) %>%
  ggplot() +
  ggtitle('Amount of trades distribution') +
  xlab('amount of trades') +
  geom_histogram(aes(n), binwidth = 1)

# show 2 and more trades clusters
g <- left_join(xdf, df, by = 't') %>%
  group_by(membership) %>%
  mutate(n = n(), dT = T - lag(T)) %>%
  filter(n > 1) %>%
  graph_from_data_frame()

plot(g,
     vertex.label.cex = .6,
     vertex.size = 1,
     edge.arrow.size = .3,
     edge.label = E(g)$dT,
     edge.label.cex = .6)

# time gap distribution
left_join(xdf, df, by = 't') %>%
  group_by(membership) %>%
  mutate(dT = T - lag(T), n = n()) %>%
  filter(n > 1) %>%
  ggplot() +
  ggtitle('Time gap distribution') +
  xlab('ms') +
  geom_histogram(aes(dT), binwidth = 5)

# found orders trades
left_join(xdf, df, by = 't') %>%
  ggplot(mapping = aes(anytime(as.numeric(substr(T, 1, 10))), q)) +
  ggtitle('Trades') +
  xlab('time') +
  ylab('quantity') +
  geom_point()

# only 0 time gap sequential orders
zt <- left_join(xdf, df, by = 't') %>%
  group_by(membership) %>%
  mutate(dT = T - lag(T), n = n()) %>%
  filter(n > 1) %>%
  filter(!(is.na(dT))) %>%
  mutate(s = sum(dT)) %>%
  filter(s == 0) %>%
  select(membership)
  
left_join(df, xdf, by = 't') %>%
  filter(membership %in% zt$membership) %>%
  select(from, to, membership, p, q, t, T)

# volume comparison
left_join(orig_df, xdf, by = 't') %>%
  mutate(v = q * p, membership = ifelse(is.na(membership), 0, 1)) %>%
  group_by(membership) %>%
  summarise(s = sum(v))
