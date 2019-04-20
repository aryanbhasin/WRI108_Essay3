## extractor: 
# load libraries
library(dplyr)
library(RedditExtractoR)
library(igraph)

# setwd("~/Desktop/Spring Sem/WRI 108/Essay 3/Data/indian elections")

# make Dataframe of Reddit thread
df = reddit_content(URL = "https://www.reddit.com/r/india/comments/aw38i5/r_megathread_iii_indiapakistan_border_skirmish/")

# extract the network
network <- df %>% user_network(include_author=FALSE, agg=TRUE)

# explore the plot
network$plot 

# construct node-list
node-list <- network$nodes
write.csv(network$nodes, file = "node-list.csv")

# construct edge-list
edge-list <- network$edges
write.csv(network$edges, file = "edge-list.csv")

# find igraph file
network$igraph



# igraph manipulation
nodes <- read.csv("node-list.csv", header = T, as.is = T)
links <- read.csv("edge-list.csv", header = T, as.is = T)
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))
colnames(links)[3] <- "weight"
colnames(nodes)[2] <- "user"

net <- graph_from_data_frame(d = links, vertices = nodes, directed = T)
class(net)

V(net)$name <- (nodes)[2]
# V(net)[[1]]$name

net
plot(net, edge.arrow.size=.4, vertex.label=NA)

# Network analysis
deg <- degree(net, mode="all")
plot(net, vertex.size=deg*3)

mean_distance(net, directed=F)
distances(net, weights=NA) # ignore weights

hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector

hist(hs, main = "Histogram of Hub Scores", xlab = "Hub Score", border = "black", col = "orange")

write.csv(hs, file = "hub-scores.csv")
write.csv(as, file = "authority-scores.csv")


hs[1:5]

plot(net, vertex.size=hs*50, main="Hubs")
plot(net, vertex.size=as*30, main="Authorities")


## End(extractor)

