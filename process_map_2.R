install.packages("grid")
install.packages("SixSigma")
install.packages("DiagrammeR")

library(grid)
library(SixSigma)
library(DiagrammeR)

df <- read.csv("new_final_enrol.csv")
df

columns <- colnames(df)

process_steps <- c("Step 1: Enrollment Initiation", 
                   "Step 2: Application Review", 
                   "Step 3: Data Entry", 
                   "Step 4: Verification", 
                   "Step 5: Resolution", 
                   "Step 6: Closure")

graph <- create_graph()

for (i in 1:length(process_steps)) {
  graph <- add_node(graph, label = process_steps[i])
}
for (i in 1:length(columns)) {
  graph <- add_node(graph, label = columns[i])
}
for (i in 1:(length(process_steps) - 1)) {
  graph <- add_edge(graph, from = process_steps[i], to = process_steps[i + 1])
}
graph <- add_edge(graph, from = process_steps[length(process_steps)], to = process_steps[1])
for (i in 1:length(columns)) {
  graph <- add_edge(graph, from = columns[i], to = process_steps[i %% length(process_steps) + 1])
}

render_graph(graph)