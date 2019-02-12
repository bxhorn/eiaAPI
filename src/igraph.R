
# 0.Initialize Code ----
library("igraph")
data.path <- "~/0-RProjects/EIAapi/data/"
##-------------------------------------------------------------------------------------------##

# 1.Data format, size, and preparation ----
# load dataset1 - edgelist
temp1 <- paste0(data.path, "Dataset1-Media-Example-NODES.csv")
temp2 <- paste0(data.path, "Dataset1-Media-Example-EDGES.csv")
nodes <- read.csv(temp1, header = T, as.is = T)
links <- read.csv(temp2, header = T, as.is = T)

# examine dataset1
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))

# collapse links of the same type and sum weights
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

# load dataset2
temp1 <- paste0(data.path, "Dataset2-Media-User-Example-NODES.csv")
temp2 <- paste0(data.path, "Dataset2-Media-User-Example-EDGES.csv")
nodes2 <- read.csv(temp1, header = T, as.is = T)
links2 <- read.csv(temp2, header = T, row.names = 1)

# examine dataset2 - matrix
head(nodes2)
head(links2)
class(links2)

# coerce links2 to matrix
links2 <- as.matrix(links2)
dim(links2)
dim(nodes2)
##-------------------------------------------------------------------------------------------##

# 2.Plotting networks with igraph ----
# The description of an igraph object starts with four letters:
# D or U, for a directed or undirected graph
# N for a named graph (where nodes have a name attribute)
# W for a weighted graph (where edges have a weight attribute)
# B for a bipartite (two-mode) graph (where nodes have a type attribute)

# The two numbers that follow (17 49) refer to the number of nodes and edges in the graph. The description also lists node & edge attributes, for example:
# (g/c) - graph-level character attribute
# (v/c) - vertex-level character attribute
# (e/n) - edge-level numeric attribute

# convert networks to igraph objects
(net <- graph_from_data_frame(d = links, vertices = nodes, directed = T))

E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
E(net)$type  # Edge attribute "type"
V(net)$media # Vertex attribute "media"

# Find nodes and edges by attribute:
net# (that returns objects of type vertex sequence/edge sequence)
V(net)[media == "BBC"]
E(net)[type == "mention"]

# You can also examine the network matrix directly:
net[1,]
net[5,7]

# get edge list and matrix
# Get an edge list or a matrix:
as_edgelist(net, names = T)
as_adjacency_matrix(net, attr = "weight")

# Or data frames describing nodes and edges:
as_data_frame(net, what = "edges")
as_data_frame(net, what = "vertices")

# plot net - not pretty
plot(net)

# remove loops and plot with edge and node formats
net <- simplify(net, remove.multiple = F, remove.loops = T)
plot(net, edge.arrow.size = .4,vertex.label = NA)

# dataset2
head(nodes2)
head(links2)

net2 <- graph_from_incidence_matrix(links2)
V(net2)
table(V(net2)$type)
plot(net2)

# plot options
plot(net, edge.arrow.size = 0.4, edge.curved = 0.1)

# Set edge color to light gray, the node & border color to orange
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff",
     vertex.label=V(net)$media, vertex.label.color="black")


# The second way to set attributes is to add them to the igraph object. Let’s say we
# want to color our network nodes based on type of media, and size them based on
# degree centrality (more links -> larger node) We will also change the width of the
# edges based on their weight.
# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]

# Compute node degrees (#links) and use that to set node size:
deg <- degree(net, mode = "all")
V(net)$size <- deg*3
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

 #change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
E(net)$width <- 1 + E(net)$weight/12
plot(net)

plot(net, edge.color = "orange", vertex.color = "gray50")

# legends
plot(net)
legend(x = -1.5, y = -1.1, c("Newspaper","Television", "Online News"), pch = 21,
       col = "#777777", pt.bg = colrs, pt.cex = 2, cex = .8, bty = "n", ncol = 1)

plot(net, vertex.shape = "none", vertex.label = V(net)$media,
     vertex.label.font = 2, vertex.label.color = "gray40",
     vertex.label.cex = .7, edge.color = "gray85")

# Let’s color the edges of the graph based on their source node color. We can get the
# starting node for each edge with the ends() igraph function. It returns the start
# and end vertex for edges listed in the es parameter. The names parameter control
# whether the function returns edge names or IDs.

edge.start <- ends(net, es = E(net), names = F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color = edge.col, edge.curved = .1)

# layouts
# Network layouts are simply algorithms that return coordinates for each node in a network.
net.bg <- sample_pa(80)
V(net.bg)$size <- 8
V(net.bg)$frame.color <- "white"
V(net.bg)$color <- "orange"
V(net.bg)$label <- ""
E(net.bg)$arrow.mode <- 0
plot(net.bg)

plot(net.bg, layout = layout_randomly)

l <- layout_in_circle(net.bg)
plot(net.bg, layout = l)

l <- layout_on_sphere(net.bg)
plot(net.bg, layout = l)

l <- layout_with_fr(net.bg)
plot(net.bg, layout = l)

# layout runs have slight variation each time. Saving the layout in l allows us to
# get the exact same result multiple times, which can be helpful if you want to plot
# the time evolution of a graph, or different relationships – and want nodes to stay
# in the same place in multiple plots.
par(mfrow = c(2,2), mar = c(0,0,0,0))   # plot four figures - 2 rows, 2 columns
plot(net.bg, layout = layout_with_fr)
plot(net.bg, layout = layout_with_fr)
plot(net.bg, layout = l)
plot(net.bg, layout = l)
dev.off()

# plot scaling
l <- layout_with_fr(net.bg)
l <- norm_coords(l, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

par(mfrow = c(2,2), mar = c(0,0,0,0))
plot(net.bg, rescale = F, layout = l*0.4)
plot(net.bg, rescale = F, layout = l*0.6)
plot(net.bg, rescale = F, layout = l*0.8)
plot(net.bg, rescale = F, layout = l*1.0)
dev.off()

plot(net.bg, rescale = F, layout = l*1.2)

##-------------------------------------------------------------------------------------------##

# eiaAPI Practice
category.list <- browse.EIA(cat.ID = 371, key)

nodesEIA <- tibble(node.id = sprintf("s%02d", 0:dim(category.list)[1]),
                       cat.id = c(-999, category.list$category_id),
                       name = c("EIA", category.list$name))
linksEIA <- tibble(from = rep("s00", dim(category.list)[1]),
                   to = pull(nodesEIA[-1, 1]),
                   type = rep("category.edge", dim(category.list)[1]))
nodesEIA <- nodesEIA[seq(dim(nodesEIA)[1],1),]
linksEIA <- linksEIA[seq(dim(linksEIA)[1],1),]

head(nodesEIA)
head(linksEIA)

(netEIA <- graph_from_data_frame(d = linksEIA, vertices = nodesEIA, directed = T))
V(netEIA)
E(netEIA)

as_edgelist(netEIA, names = FALSE)
as_adjacency_matrix(netEIA, names = FALSE)

(l <- layout_as_tree(netEIA))
l[, 2] <- l[, 1]/max(l[, 1])
l[, 1] <- c(rep(0.35, 12), -0.75)

plot(netEIA, layout = l,
     edge.arrow.size = 0,
     edge.color = "grey87",
     vertex.color = "orange",
     vertex.frame.color = "orange",
     vertex.shape = "circle",
     vertex.size = 1.25,
     vertex.label.cex = 0.95,
     vertex.label.degree = c(rep(-pi/2, dim(nodesEIA)[1] - 1), pi),
     vertex.label.dist = c(rep(0.2, dim(nodesEIA)[1] - 1), 1),
     margin = rep(0, 4),
     rescale = FALSE,
     frame = FALSE)
