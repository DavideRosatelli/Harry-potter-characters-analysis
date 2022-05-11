library(tidyverse)
library(igraph)
library(ggraph)
library(network)
library(tidygraph)

data <- read.csv('Harry Potter network.csv')
data <- data[-c(1,4)]

sources <- data %>%
  distinct(source) %>%
  rename(label = source)

destinations <- data %>%
  distinct(target) %>%
  rename(label = target)

nodes <- full_join(sources, destinations, by = "label")

nodes <- nodes %>% rowid_to_column("id")

per_route <- data %>%  
  group_by(source, target) %>%
  summarise(weight = n()) %>% 
  ungroup()

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("target" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(alpha = 0.8, edge_width=0.2) +
  geom_node_point( color="#69b3a2", size=5) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), , angle=65, hjust=0.3, nudge_y=-1, size=2,5) +
  labs(edge_width = "Letters") +
  theme_graph()