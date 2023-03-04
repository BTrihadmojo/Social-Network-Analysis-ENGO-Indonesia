setwd("")

# load required packages
library(igraph)
library(ggplot2)

# read the Excel file
initial_network <- read.csv("initial network.csv")

# print column names
print(colnames(initial_network))

# remove leading and trailing white spaces from the name column
initial_network$name <- trimws(initial_network$name)

# create an empty graph
g <- graph.empty(n = nrow(initial_network), directed = FALSE)

# add vertices
set_vertex_attr(g, "name", value = as.character(initial_network$name))
set_vertex_attr(g, "position", value = initial_network$position)
set_vertex_attr(g, "organization", value = initial_network$Organization)
set_vertex_attr(g, "type", value = initial_network$Type)

# print initial_network data frame
print(initial_network)

# print graph vertices
print(V(g)$name)

# define edges
edges <- data.frame(name = initial_network$name, organization = initial_network$Organization)
edges$name <- as.factor(trimws(edges$name))

# print edges data frame
print(edges)

# find common vertex names between edges and initial_network data frames
common_names <- intersect(edges$name, initial_network$name)

# subset edges data frame
edges <- edges[edges$name %in% common_names, ]

# print subsetted edges data frame
print(edges)

# convert vertex names to indices
edges <- edges$name
edge_indices <- match(edges, V(g)$name)

# print edge indices
print(edge_indices)

# add edges
add_edges(g, edge_indices)
set_edge_attr(g, "relationship", value = initial_network$relationship)

# assign vertex shapes
shape_map <- c("Individual" = "circle", "ENGO" = "square",
               "Research Center" = "triangle", "Corporation" = "diamond",
               "Bank" = "triangle", "University" = "square", "Philanthropy" = "diamond")
vertex_shapes <- shape_map[match(initial_network$Type, names(shape_map))]
set_vertex_attr(g, "shape", value = vertex_shapes)

# calculate vertex positions
lay <- layout_with_fr(g)

# plot the network
print(unique(initial_network$Organization))
plot(g, layout = lay, vertex.label = levels(factor(V(g)$name)),
     vertex.color = as.character(V(g)$organization),
     vertex.shape = V(g)$shape,
     vertex.label.cex = 0.8,
     edge.color = "#555555",
     edge.width = 0.5,
     edge.arrow.size = 0.5,
     main = "Initial Network",
     xlab = "", ylab = "")

# add legend
legend("topright", legend = levels(factor(V(g)$organization)),
       col = unique(as.character(V(g)$organization)),
       pch = 21, pt.bg = unique(as.character(V(g)$organization)), pt.cex = 2,
       cex = 0.8, title = "Organization")

# Compute some network metrics
degree(g) # degree of each node
betweenness(g) # betweenness centrality of each node

