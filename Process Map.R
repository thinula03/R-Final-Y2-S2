
df <- read.csv("F:\\new_final_enrol.csv")
df

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

plot(process_graph, edge.arrow.size = 0.5, edge.curved = TRUE, vertex.size = 10, vertex.label.cex = 0.6)

print(V(process_graph)$label)

opened_vertex <- which(V(process_graph)$label == "Opened_At")
closed_vertex <- which(V(process_graph)$label == "Closed_At")

print(c(opened_vertex, closed_vertex))

if (length(opened_vertex) == 0 || length(closed_vertex) == 0) {
  stop("Vertices not found.")
}

print(c(opened_vertex, closed_vertex))
