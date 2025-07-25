

stat_network <- function(data){
  
  library(igraph)
  submatr <- data[data$pval<0.05 & abs(data$pSC)>0.2,]
  
  pSC_matrix <- submatr[,c("feat1","feat2","pSC")]
  input_data <- reshape2::dcast(pSC_matrix, feat1~feat2); nrow(input_data); ncol(input_data)
  rownames(input_data) <- input_data$feat1; input_data <- input_data[,-1] 
  input_data[is.na(input_data)] <- 0
  for(i in 1:nrow(input_data)){input_data[i,i] <- 1}
  
  
  #' @:transitivity------------------- 
  #' @:nodes-file                             
  num_nodes1 <- data.frame(table(submatr$feat1))
  num_nodes2 <- data.frame(table(submatr$feat2))
  num_nodes <- rbind(num_nodes1, num_nodes2) 
  
  nodes <- data.frame(node=unique(c(submatr$feat1, submatr$feat2))) 
  nodes <- merge(nodes, num_nodes, by.x = "node", by.y = "Var1", all.x=T)
  nodes <- aggregate(nodes$Freq, by=list(nodes$node), sum)
  names(nodes) <- c("node","Freq")
  nodes <- nodes[order(nodes$Freq, decreasing = T),]
  nodes <- rbind(nodes, data.frame(node="KSD", Freq=1))
  
  nodes$seq <- seq(0,nrow(nodes)-1) 
  nodes$angle <- 90 - 360 * (nodes$seq+0.5) / nrow(nodes)
  nodes$angle <- ifelse(nodes$angle < (-90), nodes$angle+180, nodes$angle)
  nodes$hjust <- ifelse(nodes$seq < round(nrow(nodes)/2), 0, 1) 
  
  #' @:edges
  edges <- data.frame(from=submatr$feat1, to=submatr$feat2, corr=submatr$pSC, qval=submatr$qval)
  edges$class <- ifelse(edges$corr>0, "Positive correlation", "Negative correlation")
  edges$class[edges$qval>0.05] <- "Insignificant"
  
  #' @:merge
  g <- tidygraph::tbl_graph(nodes=nodes, edges=edges)
  
  graph <- graph_from_data_frame(edges, vertices = data.frame(name = nodes$node), directed = TRUE)
  global_transitivity <- transitivity(graph, type = "barrat")
  global_transitivity[is.na(global_transitivity)] <- 0
  global_transitivity <- mean(global_transitivity)
  
  
  #' @:Global-toplogical-features------------------
  g1 <- graph.adjacency(as.matrix(input_data), weighted=TRUE, mode="undirected")
  g1 <- simplify(g1)
  node.label <- V(g1)$name
  
  print("Extract global toplogical features!")
  num.nodes <- vcount(g1) # node number
  num.edges <- ecount(g1) # edge number
  num.pos.edges <- sum(E(g1)$weight>0) # number of positive correlation edges
  num.neg.edges <- sum(E(g1)$weight<0) # number of negative correlation edges
  global.degree <- mean(degree(g1)) # global average degree
  global.density <- edge_density(g1, loops=FALSE) # Density
  global.diameter <- diameter(g1,directed = F,weights = NA) # Overall path
  global.edge.connecivity <- edge_connectivity(g1)
  global.cluster.coef <- transitivity(g1, type = "average") # Average clustering coefficient
  
  E(g1)$weight = abs(E(g1)$weight)
  global.average.path <- mean_distance(g1) # Average path length
  g2 <- g1
  global.clossness <- mean(closeness(g1)) # Closeness centrality
  global.betweeness <- mean(betweenness(g1)) # Betweeness centrality
  global.eigen.centrality <- mean(evcent(g1)$vector) # eigen_centrality
  
  
  #' @:output----------------------
  result <- data.frame(num.nodes=num.nodes,
                       num.edges=num.edges,
                       num.pos.edges=num.pos.edges,
                       num.neg.edges=num.neg.edges,
                       global.degree=global.degree,
                       global.density=global.density,
                       global.diameter=global.diameter,
                       global.edge.connecivity=global.edge.connecivity,
                       global.cluster.coef=global.cluster.coef,
                       global.average.path=global.average.path,
                       global.clossness=global.clossness,
                       global.betweeness=global.betweeness,
                       global.eigen.centrality=global.eigen.centrality,
                       global_transitivity=global_transitivity)
  return(result)
}
