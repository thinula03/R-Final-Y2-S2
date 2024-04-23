df <- read.csv("new_final_enrol.csv")
df

install.packages("igraph")
library(igraph)

df$Opened_At <- as.POSIXct(df$Opened_At, format = "%Y-%m-%d %H:%M:%S")
df$Application_Created_At <- as.POSIXct(df$Application_Created_At, format = "%Y-%m-%d %H:%M:%S")
df$Last_Updated_At <- as.POSIXct(df$Last_Updated_At, format = "%Y-%m-%d %H:%M:%S")
df$Resolved_At <- as.POSIXct(df$Resolved_At, format = "%Y-%m-%d %H:%M:%S")
df$Closed_At <- as.POSIXct(df$Closed_At, format = "%Y-%m-%d %H:%M:%S")

column_names <- colnames(df)
process_steps <- paste("Step", 1:length(column_names), ":", column_names)
edge_list <- t(combn(column_names, 2))
process_graph <- graph_from_edgelist(as.matrix(edge_list), directed = TRUE)
V(process_graph)$label <- process_steps
plot(process_graph, edge.arrow.size = 0.5, edge.curved = TRUE, vertex.size = 10, vertex.label.cex = 0.8)

all_paths <- all_simple_paths(process_graph, from = "Opened_At", to = "Closed_At", mode = "out")
print(all_paths)

optimal_path <- shortest_paths(process_graph, from = "Opened_At", to = "Closed_At", mode = "out")
print(optimal_path)


optimal_path_vertices <- V(process_graph)[optimal_path$vpath[[1]]]$label
print(optimal_path_vertices)



all_paths <- all_simple_paths(process_graph, from = "Opened_At", to = "Closed_At", mode = "out")

shortest_length <- Inf
shortest_path <- NULL

for (path in all_paths){
  path_length <- length(path)
  
  if (path_length < shortest_length){
    shortest_length <- path_length
    
    shortest_path <- path
  }
}

optimal_path_vertices <- V(process_graph)[shortest_path]$label
print(optimal_path_vertices)










