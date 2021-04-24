library(igraph) 
graph1 <- graph(edges = c(1,2, 2,3, 3,4, 4,5, 1,5), n = 5) #Assign edges and number of vertices
graph1 <- graph(edges = c(1,2, 2,3, 3,4, 4,5, 1,5, 2,4, 3,5, 5,2, 1,4, 2,4, 3,1), n =5)
plot(graph1) #Plot the network


graph2 <- graph (edges=c(1,2, 2,3, 3,1), n = 3, directed = F)
plot(graph2) 
E(graph2) 


graph3 <- graph(edges = c(1,2, 2,3, 3,1, 5,6, 6,9, 9,13, 13,2, 5,1), n = 15, directed = T)
plot(graph3)


graph4 <- graph( c("1", "2",
                   "2", "3",
                   "2", "3",
                   "1", "1"), 
                 isolates=c("4", "5", "6", "7") )  
plot(graph4, edge.arrow.size=.25, vertex.color="gold", vertex.size=15,  #Plotting asthetics
     vertex.frame.color="gold", vertex.label.color="black", 
     vertex.label.cex=0.8, vertex.label.dist=0, edge.curved=.5, edge.color = "black") 

Bridgegraph <- graph( c("Bridge1","Bridge2", "Bridge3","Bridge4", "Bridge2","Bridge3", "Bridge1", "Bridge4","Bridge5", "Bridge3","Bridge5", "Bridge5","Bridge7")) # named vertices
plot(Bridgegraph)

plot(graph_from_data_frame(iris), edge.color = iris$Species,
     , edge.arrow.size=.15, vertex.size=10, vertex.color="gray")
