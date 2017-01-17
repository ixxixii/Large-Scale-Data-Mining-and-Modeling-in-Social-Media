library(igraph)
filePath = "/Users/JohnZ/Desktop/facebook_combined.txt"
g = read.graph(filePath,format = "ncol",directed = FALSE)


#--------------------------Problem 1--------------------------#
connectivity = is.connected(g)
directivity = is.directed(g)
d = diameter(g)
deg = degree(g)
h = hist(deg, breaks=seq(0.0, by=1 , length.out=max(degree(g))+2))  #will plot a graph of degree distribution     
df = data.frame(x=h$mids, y=h$density)
plot(df, type="h",main="Degree Distribution of Facebook Graph", xlab="Degree", ylab="Density")

#Using fitdist from liarary(MASS) normal is the best which is still bad fitting.

library("ggplot2")
x=h$mids
y=h$density
models <- list(
  nls(y ~ I((1/x*a) + b*x), data = df, start = list(a = 1, b = 0),trace=T), 
  nls(y ~ I((a + b*log(x))), data=df, start = list(a = 0, b = 0),trace=T),
  nls(y ~ I((exp(a + b * x))), data=df, start = list(a=0,b=0),trace=T),
  nls(y ~ I((1/x*a)+b), data=df, start = list(a=1,b=0),trace=T))


ggplot(df, aes(x, y)) + geom_point(size = 2) +
  geom_line(aes(x,fitted(models[[1]])), size = 1,colour = "blue") + 
  geom_line(aes(x,fitted(models[[2]])), size = 1, colour = "yellow") +
  geom_line(aes(x,fitted(models[[3]])),size = 1,  colour = "red") +
  geom_line(aes(x,fitted(models[[4]])),size = 1,  colour = "purple")+
  ggtitle("Fitted curves for degree distribution")+ xlab("Nodes") +ylab("Degree Distribution")

summary(models[[3]])
models[[3]]
#Q total mean square error is residual ?

avg_deg = mean(degree(g))


#--------------------------Problem 2--------------------------#
personnetwork_1 = neighborhood(g, 1, 1)  # why the ID is 1 instead of 0 ?
personnetwork_nodes = personnetwork_1[[1]]
net1nodesNum = length(personnetwork_nodes)
subgraph_1 = induced_subgraph(g,personnetwork_nodes)
length(E(subgraph_1))  #2866
length(V(subgraph_1))  #348


#--------------------------Problem 3--------------------------#
deg = degree(g)
coreNodes = which(neighborhood.size(g, 1 , nodes=V(g)) > 201) #200+1(coreNode itself)
coreNodesNum = length(coreNodes)
avg_coreNodesdeg = mean(deg[coreNodes])

coreNode = coreNodes[6]
net3 = neighborhood(g, 1, nodes=coreNode)
net3_nodes = net3[[1]]
subgraph_3 = induced_subgraph(g,net3_nodes)
length(E(subgraph_3))  #4525
length(V(subgraph_3))  #232


coreNodeinsubgraph3 = which(neighborhood.size(subgraph_3, 1 , nodes=V(subgraph_3)) == length(V(subgraph_3)))

vertexSizeVector = rep(2,vcount(subgraph_3))
vertexSizeVector[V(subgraph_3)[coreNodeinsubgraph3]] = 5

#plot(subgraph_3,main="Subgraph of the coreNode_6",vertex.label=NA,vertex.size=vertexSizeVector,asp=9/16)
getwd()
jpeg("Problem3_Network_CoreNode_6.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_3 ,main="Subgraph of coreNode_6 with 232 nodes and 4525 edges", vertex.size=vertexSizeVector , vertex.label=NA , asp=9/16)
dev.off()

#Fast-Greedy
fg = fastgreedy.community(subgraph_3)
fgNum = max(fg$membership)

#plot(fg,subgraph_3,main="Communities Structure for CoreNode_6,ComNum=3,Fast-Greedy",vertex.size=vertexSizeVector,vertex.label=NA,asp=9/16)

getwd()
jpeg("Communities Structure for CoreNode_6,ComNum=3,Fast-Greedy.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_3 , vertex.size=vertexSizeVector , vertex.label=NA , vertex.color=fg$membership,asp=9/16,
     main="Communities Structure for CoreNode_6,ComNum=3,Fast-Greedy")
dev.off()

#Edge-Betweenness
eb = edge.betweenness.community(subgraph_3)
ebNum = max(eb$membership)

#plot(eb,subgraph_3,main="Communities Structure for CoreNode_6,ComNum=10,Edge-Betweenness",vertex.size=vertexSizeVector,vertex.label=NA)

getwd()
jpeg("Communities Structure for CoreNode_6,ComNum=10,Edge-Betweenness.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_3 , vertex.size=vertexSizeVector , vertex.label=NA , vertex.color=eb$membership,asp=9/16,
     main="Communities Structure for CoreNode_6,ComNum=10,Edge-Betweenness")
dev.off()


im <- infomap.community(subgraph_3)
imnum = max(im$membership)

#plot(im,subgraph_3,main="Communities Structure for CoreNode_6,ComNum=4,Infomap",vertex.size=vertexSizeVector,vertex.label=NA)

getwd()
jpeg("Communities Structure for CoreNode_6,ComNum=4,Infomap.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_3 , vertex.size=vertexSizeVector , vertex.label=NA , vertex.color=im$membership,asp=9/16,
     main="Communities Structure for CoreNode_6,ComNum=4,Infomap")
dev.off()


#--------------------------Problem 4--------------------------#
coreNodeinsubgraph4 = which(neighborhood.size(subgraph_3, 1 , nodes=V(subgraph_3)) == length(V(subgraph_3)))
subgraph_4 = delete_vertices(subgraph_3, V(subgraph_3)[coreNodeinsubgraph4]) 
length(E(subgraph_4))  
length(V(subgraph_4)) 
is.connected(subgraph_4)
vertexSizeVector = rep(2,vcount(subgraph_3))

#plot(subgraph_4,main="Subgraph of the coreNode_6(coreNode removed)",vertex.label=NA,vertex.size=vertexSizeVector)
getwd()
jpeg("Problem4_Network_CoreNode_6.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_3 ,main="Subgraph of the coreNode_6(coreNode removed)", vertex.size=vertexSizeVector , vertex.label=NA , asp=9/16)
dev.off()

#Fast-Greedy
fg4 = fastgreedy.community(subgraph_4)
fgNum = max(fg4$membership)

#plot(fg4,subgraph_4,main="Communities Structure for CoreNode_6(coreNode removed),ComNum=3,Fast-Greedy",vertex.size=vertexSizeVector,vertex.label=NA)

jpeg("Communities Structure for CoreNode_6(coreNode removed),ComNum=3,Fast-Greedy.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_4 , vertex.size=vertexSizeVector , vertex.label=NA , vertex.color=fg4$membership,asp=9/16,
     main="Communities Structure for CoreNode_6(coreNode removed),ComNum=3,Fast-Greedy")
dev.off()

#Edge-Betweenness
eb4 = edge.betweenness.community(subgraph_4)
ebNum = max(eb4$membership)

#plot(eb4,subgraph_4,main="Communities Structure for CoreNode_6(coreNode removed),ComNum=11,Edge-Betweenness",vertex.size=3,vertex.label=NA)

jpeg("Communities Structure for CoreNode_6(coreNode removed),ComNum=11,Edge-Betweenness.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_4 , vertex.size=vertexSizeVector , vertex.label=NA , vertex.color=eb4$membership,asp=9/16,
     main="Communities Structure for CoreNode_6(coreNode removed),ComNum=11,Edge-Betweenness")
dev.off()


im4 <- infomap.community(subgraph_4)
imnum = max(im4$membership)

plot(im4,subgraph_4,main="Communities Structure for CoreNode_6(coreNode removed),ComNum=4,Infomap",vertex.size=vertexSizeVector,vertex.label=NA)

jpeg("Communities Structure for CoreNode_6(coreNode removed),ComNum=4,Infomap.png",width=2400,height=2000)
par(cex = 3)
plot(subgraph_4 , vertex.size=vertexSizeVector , vertex.label=NA , vertex.color=im4$membership,asp=9/16,
     main="Communities Structure for CoreNode_6(coreNode removed),ComNum=4,Infomap")
dev.off()


#--------------------------Problem 5--------------------------#
commNeigb <- function(g,u,v){
  Neigb_u = neighbors(g,u)
  Neigb_v = neighbors(g,v)
  intersect(Neigb_u,Neigb_v)
}

#embeddedness
embeddedness <- function(g,u,v){
  length(commNeigb(g,u,v))
}
  
#embeddedness2 <- function(g,u){      #g is the selected personal network and u is a node in this network
#  embedtemp = neighbors(g,u)-1 
#}

personnetwork <- function(g,ordr,coreNode){
  net = neighborhood(g, ordr, nodes=coreNode)
  net_nodes = net[[1]]
  subgraph = induced_subgraph(g,net_nodes)
}

#disperson
dispersion <- function(g,u,v){
  commNeighbor = commNeigb(g,u,v)
  g_removed = delete_vertices(g, c(u,v))
  disp = 0
  for (f in commNeighbor) {
    for (t in commNeighbor) {
      if(f!=t) {
        temp = get.shortest.paths(g_removed,from=f,to=t)
      if(length(temp$vpath[[1]])>0){
        distance = length(temp$vpath[[1]])-1   # temp$vpath[[1]] = list of ids of nodes in the shortest path
        disp = disp + distance
      }
      }
    }
  }
  disp = disp/2  # double calculate 
}

ratiodispembed <- function(d,e){
  ratio = d/e
}

#plot community structures with highlighted max embeddedness,maxdispersion,maxratio seperately and all together
embed_disp_rat_community <- function(g,coreNode){
  
  embed_max = 0
  maxembed_node = 0
  disp_max = 0
  maxdisp_node = 0
  rat_max = 0
  maxrat_node = 0
  
  subgraph_5 = personnetwork(g, 1, coreNode)
  coreNodeinsubgraph5 = which(neighborhood.size(subgraph_5, 1 , nodes=V(subgraph_5)) == length(V(subgraph_5)))
  
  for (v in V(subgraph_5)) {
    if(v!=coreNodeinsubgraph5){
      embed = embeddedness(subgraph_5,V(subgraph_5)[coreNodeinsubgraph5],V(subgraph_5)[v])
      disp = dispersion(subgraph_5,V(subgraph_5)[coreNodeinsubgraph5],V(subgraph_5)[v])
    
      if(embed > embed_max){
        embed_max = embed 
        maxembed_node = v
      }
      if(disp > disp_max){
        disp_max = disp
        maxdisp_node = v
      }
      if (embed>0){
        rat = ratiodispembed(disp,embed)
        if(rat>rat_max){
          rat_max = rat
          maxrat_node = v
        }
      }

      fg5 = fastgreedy.community(subgraph_5)
      fgNum = max(fg5$membership)
      fgNum
      
#plot community with highlight max embeddedness only      
      if(maxembed_node > 0){
        vertexSizeVector = rep(2,vcount(subgraph_5))
        vertexColorVector = fg5$membership
        vertexLabelVector = rep(NA,vcount(subgraph_5))
        
        vertexSizeVector[V(subgraph_5)[maxembed_node]] = 5
        vertexColorVector[V(subgraph_5)[maxembed_node]] = 'red1'
        vertexLabelVector[V(subgraph_5)[maxembed_node]] = 'Maxembed_node'
        
        getwd()
        jpeg("PersonalNetwork_Community_Structure_Node_With_MaxEmbeddedness_Highlighted.png",width=2400,height=2000)
        par(cex = 3)
        plot(subgraph_5 ,main="PersonalNetwork_Community_Structure_Node_With_MaxEmbeddedness_Highlighted", 
             vertex.size=vertexSizeVector, vertex.label=vertexLabelVector, vertex.color=vertexColorVector, asp=9/16)
        dev.off()
      }
      
      if(maxdisp_node > 0){
        vertexSizeVector = rep(2,vcount(subgraph_5))
        vertexColorVector = fg5$membership
        vertexLabelVector = rep(NA,vcount(subgraph_5))
        
        vertexSizeVector[V(subgraph_5)[maxdisp_node]] = 5
        vertexColorVector[V(subgraph_5)[maxdisp_node]] = 'yellow1'
        vertexLabelVector[V(subgraph_5)[maxdisp_node]] = 'Maxdisp_node'
 
        jpeg("PersonalNetwork_Community_Structure_Node_With_MaxDispersion_Highlighted.png",width=2400,height=2000)
        par(cex = 3)
        plot(subgraph_5 ,main="PersonalNetwork_Community_Structure_Node_With_MaxDispersion_Highlighted", 
             vertex.size=vertexSizeVector, vertex.label=vertexLabelVector, vertex.color=vertexColorVector, asp=9/16)
        dev.off()
      }
      if(maxrat_node > 0){
        vertexSizeVector = rep(2,vcount(subgraph_5))
        vertexColorVector = fg5$membership
        vertexLabelVector = rep(NA,vcount(subgraph_5))
        
        vertexSizeVector[V(subgraph_5)[maxrat_node]] = 5
        vertexColorVector[V(subgraph_5)[maxrat_node]] = 'green1'
        vertexLabelVector[V(subgraph_5)[maxrat_node]] = 'Maxratio_node'
        
        jpeg("PersonalNetwork_Community_Structure_Node_With_Maxratio_Of_Dispersion_And_Embeddedness_Highlighted.png",width=2400,height=2000)
        par(cex = 3)
        plot(subgraph_5 ,main="PersonalNetwork_Community_Structure_Node_With_Maxratio_Of_Dispersion_And_Embeddedness_Highlighted", 
             vertex.size=vertexSizeVector, vertex.label=vertexLabelVector, vertex.color=vertexColorVector, asp=9/16)
        dev.off()
      }
      if((maxrat_node > 0)&& (maxrat_node > 0) && (maxdisp_node)){
        vertexSizeVector = rep(2,vcount(subgraph_5))
        vertexColorVector = fg5$membership
        vertexLabelVector = rep(NA,vcount(subgraph_5))
        
        vertexSizeVector[V(subgraph_5)[maxembed_node]] = 5
        vertexColorVector[V(subgraph_5)[maxembed_node]] = 'red1'
        vertexLabelVector[V(subgraph_5)[maxembed_node]] = 'Maxembed_node'
        vertexSizeVector[V(subgraph_5)[maxdisp_node]] = 5
        vertexColorVector[V(subgraph_5)[maxdisp_node]] = 'yellow1'
        vertexLabelVector[V(subgraph_5)[maxdisp_node]] = 'Maxdisp_node'
        vertexSizeVector[V(subgraph_5)[maxrat_node]] = 5
        vertexColorVector[V(subgraph_5)[maxrat_node]] = 'green1'
        vertexLabelVector[V(subgraph_5)[maxrat_node]] = 'Maxratio_node'
        
        jpeg("PersonalNetwork_Community_Structure_Node_With_MaxEmbeddedness_Or_MaxDispersion_Or_Maxratio_Highlighted.png",width=2400,height=2000)
        par(cex = 3)
        plot(subgraph_5 ,main="PersonalNetwork_Community_Structure_Node_With_Maxratio_Of_Dispersion_And_Embeddedness_Highlighted", 
             vertex.size=vertexSizeVector, vertex.label=vertexLabelVector, vertex.color=vertexColorVector, asp=9/16)
        dev.off()
      }
    }
  }
}


# plot the hist of embeddedness and dispersion over all personal networks
embed_total = numeric(0)
disp_total = numeric(0)

for (coreNode in coreNodes) {
  subgraph_5 = personnetwork(g, 1, coreNode)
  coreNodeinsubgraph5 = which(neighborhood.size(subgraph_5, 1 , nodes=V(subgraph_5)) == length(V(subgraph_5)))
  # this formular has a problem that a neighbor of coreNode could also connected to all nodes in the subgraph
  for (v in V(subgraph_5)) {
    if(v!=coreNodeinsubgraph5){
      embed = embeddedness(subgraph_5,V(subgraph_5)[coreNodeinsubgraph5],V(subgraph_5)[v])
      disp = dispersion(subgraph_5,V(subgraph_5)[coreNodeinsubgraph5],V(subgraph_5)[v])
      
      embed_total = c(embed_total,embed)
      disp_total = c(disp_total,disp)
    }
  }
}

hist (embed_total, breaks=seq (-0.5, by=1, length.out=max(embed_total) +2), main ="Embeddedness Distribution Over All Personal Networks ", xlab="embd")
hist (disp_total, breaks=seq (-0.5, by=1, length.out=max(disp_total) +2), main="Dispersion Distribution Over All Personal Networks", xlab="dip")









test3 = 0
for (v in V(subgraph_5)) {
  if(v==1){
    test3 = V(subgraph_5)[v]
    test4 = v
    }
  else{
    test3 = c(test3,V(subgraph_5)[v])
    test4 = c(test4,v)
  }  
}
V(subgraph_5)[1]
V(subgraph_5)[2]

for (coreNode in coreNodes) {
  if(coreNode==1)test6=coreNode
  else test6 = c(test6,coreNode)
}

nodetest = V(subgraph_5)

for (v in nodetest) {
  if(v==1)test7=v
  else test7 = c(test7,v)
}
