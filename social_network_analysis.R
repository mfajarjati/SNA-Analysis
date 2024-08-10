

library(igraph)		# untuk melakukan analisis dan visualisasi graf
library(tidyverse)	# kumpulan fungsi untuk manipulasi dan analisis data
library(dplyr)		# sejumlah fungsi untuk melakukan operasi transformasi data
library(tidygraph)	# menyediakan fungsi untuk memanipulasi dan menganalisis graf
library(ggraph)		# membuat visualisasi graf yang menarik dan informatif

# mengambil data
data <-
  read.csv("D:/coding_tweet_asli.csv",
           header = T)


# memfilter data dan menyusun edge-nya
edge_df <- data %>%
  select(author, retweeted_screen_name) %>%
  mutate(
    retweeted_screen_name = str_replace_all(
      string = retweeted_screen_name,
      pattern =  "^c\\(|\\)$",
      replacement = ""
    )
  ) %>%
  separate_rows(retweeted_screen_name, sep = ",") %>%
  mutate(
    retweeted_screen_name = str_replace_all(
      string = retweeted_screen_name,
      pattern =  "[[:punct:] ]+",
      replacement = ""
    )
  ) %>%
  filter(retweeted_screen_name != "") %>%
  slice(1:1000) %>%
  rename(to = author, from = retweeted_screen_name)
view(edge_df)

#menyimpan file setelah di filter
write.csv(edge_df, "data_filter.csv")

# membuat node berdasarkan 2 kolom
nodes_df <- data.frame(name = unique(c(edge_df$from, edge_df$to)),
                       stringsAsFactors = F)
tail(nodes_df)

# menentukan jenis graf
graph_tweets <- tbl_graph(nodes = nodes_df,
                          edges = edge_df,
                          directed = F)

#menghitung degree tertinggi
graph_tweets <- graph_tweets %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(normalized = TRUE),
    closeness = centrality_closeness(),
    eigenvector = centrality_eigen()
  ) %>%
  arrange(desc(degree))

network_act_df <- graph_tweets %>%
  activate(nodes) %>%
  as.data.frame()

head(network_act_df)

#menghitung betwenness tertinggi
network_act_df <- graph_tweets %>%
  activate(nodes) %>%
  as.data.frame() %>%
  arrange(desc(betweenness))

head(network_act_df)

#menghitung closeness tertinggi
graph_tweets <- graph_tweets %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(normalized = TRUE),
    closeness = centrality_closeness(),
    eigenvector = centrality_eigen()
  ) %>%
  arrange(desc(closeness))

network_act_df <- graph_tweets %>%
  activate(nodes) %>%
  as.data.frame()

head(network_act_df)

#menghitung Eigenvector tertinggi
graph_tweets <- graph_tweets %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),
    betweenness = centrality_betweenness(normalized = TRUE),
    closeness = centrality_closeness(),
    eigenvector = centrality_eigen()
  ) %>%
  arrange(desc(eigenvector))

network_act_df <- graph_tweets %>%
  activate(nodes) %>%
  as.data.frame()

head(network_act_df)

# mengurutkan berdasarkan centralit-nya masing-masing
kp_activity <- data.frame(
  network_act_df %>% arrange(-degree) %>% select(name) %>% slice(1:6),
  network_act_df %>% arrange(-betweenness) %>% select(name) %>% slice(1:6),
  network_act_df %>% arrange(-closeness) %>% select(name) %>% slice(1:6),
  network_act_df %>% arrange(-eigenvector) %>% select(name) %>% slice(1:6)
) %>% setNames(c("degree", "betweenness", "closeness", "eigenvector"))
kp_activity

# melihat isi tweet
data %>%
  filter(retweeted_screen_name == "NabilHussein") %>%
  arrange(desc(retweeted_screen_name)) %>%
  distinct(title) %>%
  pull(title)

data %>%
  filter(retweeted_screen_name == "omarqe") %>%
  arrange(desc(retweeted_screen_name)) %>%
  distinct(title) %>%
  pull(title)

# memvisualisasikan graf
set.seed(123)
graph_tweets <- graph_tweets %>%
  activate(nodes) %>%
  mutate(community = group_louvain()) %>%
  activate(edges) %>%
  filter(!edge_is_loop())

graph_tweets %>%
  activate(nodes) %>%
  as.data.frame() %>%
  count(community)

important_user <- function(data) {
  name_person <- data %>%
    as.data.frame() %>%
    filter(community %in% 1:5) %>%
    select(-community) %>%
    pivot_longer(-name, names_to = "measures", values_to = "values") %>%
    group_by(measures) %>%
    arrange(desc(values)) %>%
    slice(1:6) %>%
    ungroup() %>%
    distinct(name) %>%
    pull(name)
  
  return(name_person)
}
important_person <-
  graph_tweets %>%
  activate(nodes) %>%
  important_user()

set.seed(13)
graph_tweets %>%
  activate(nodes) %>%
  mutate(ids = row_number(),
         community = as.character(community)) %>%
  filter(community %in% 1:5) %>%
  arrange(community, ids) %>%
  mutate(node_label = ifelse(name %in% important_person, name, NA)) %>%
  ggraph(layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(
    aes(size = degree, fill = community),
    shape = 21,
    alpha = 0.7,
    color = "grey30"
  ) +
  geom_node_label(
    aes(label = node_label),
    repel = TRUE,
    alpha = 0.8,
    max.overlaps = 10
  ) +
  guides(size = "none") +
  labs(title = "top 5 tweet menggunakan kata 'coding'",
       color = "Interaction",
       fill = "Community") +
  theme_void() +
  theme(legend.position = "top")