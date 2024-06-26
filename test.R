# Load the igraph library
# Well bro, you know what I mean. This is written when I am trying to connect my Rstudio to Github
# And sincerely, this is cool.
library(igraph)
library(visNetwork)

# read the edge-list file and plot the graph
file_path <- "/Users/xingxing/Desktop/大学生活/2024暑期申请/Big Data Institute/symptom_edgelist.csv"
edge_list <- read.csv(file_path, header = TRUE)
graph <- graph_from_data_frame(edge_list, directed = FALSE)
E(graph)$weight <- edge_list$weight
plot(graph, edge.width = E(graph)$weight * 100, main = "The symptom network of schistosomiasis disease")
# 79 vertices, 141 edges
degree_values <- degree(graph)
max_degree <- max(degree_values)
max_degree_vertices <- V(graph)$name[degree_values == max_degree]
# Plot a histogram showcasing the degree distribution of the graph
hist(degree_values,
     breaks = seq(0,max(degree_values) + 1, by = 1),
     main = "Degree Distribution of schistosomiasis disease states.",
     xlab = "Degree",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")
# Calculating some centrality measures of the graph
betweenness_centrality <- betweenness(graph)

num_edge <- ecount(graph)
num_vertex <- vcount(graph)
density <- (num_edge*2)/(num_vertex * (num_vertex -1 ))
print(density)
# Graphing the betweenness centrality distribution
hist(betweenness_centrality,
     breaks = seq(0, max(betweenness_centrality) + 1, by = 1),
     main = "Betweenness Centrality Distribution",
     ylim =  c(0,5),
     xlab = "Centrality",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")

# Carry out the Louvain Clustering Algorithm
set.seed(15)
communities <- cluster_louvain(graph)
membership <- membership(communities)


# Plot the graph with vertices colored by community membership 

nodes <- data.frame(id = V(graph)$name, label = "", title = paste("Node ID:", V(graph)$name), group = membership)
edges <- get.data.frame(graph, what = "edges")
# Use the visNetwork library to create interactive plots
louvain_graph <- visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE)) %>%
  visLayout(randomSeed = 60) %>%
  visNodes(font = list(size = 30))
louvain_graph

# Implement the spectral clustering
Laplacian<- laplacian_matrix(graph)
eigen_decomp <- eigen(Laplacian)
num_clusters <- 11
selected_vectors <- eigen_decomp$vectors[, (ncol(eigen_decomp$vectors) - num_clusters + 1):ncol(eigen_decomp$vectors)]

clustering <- kmeans(selected_vectors, centers = num_clusters, nstart = 10)
spect_membership <- clustering$cluster
V(graph)$cluster <- as.factor(clustering$cluster)

nodes <- data.frame(id = V(graph)$name, label = "", title = paste("Node ID:", V(graph)$name), group = spect_membership)
edges <- get.data.frame(graph, what = "edges")
# Again plotting the graph with visNetwork to make it interactive
visNetwork(nodes, edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE)) %>%
  visLayout(randomSeed = 60) %>%
  visNodes(font = list(size = 30))
