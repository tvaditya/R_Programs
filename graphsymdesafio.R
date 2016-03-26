library(igraph)

#Importar o arquivo edges, após ter convertido para o formato CSV
edges <- read.table("C:/symdesafio/edges.csv", quote="\"", comment.char="")

#Usando igraph para gerar o grafo, convertendo o data.frame edges para uma matriz
el <- as.matrix(edges) 
el[,1]=as.character(el[,1])
el[,2]=as.character(el[,2])

# Gerando o grafo g com a matriz gerada a partir da lista de edges
g <- graph.edgelist(el,directed=FALSE)

# Gerando uma matriz de adjacencia
adjmat <- get.adjacency(g,sparse=FALSE)

# Gerando uma lista de adjacencia
adjlist <- get.adjlist(g)


#Plotando o grafo gerado
plot(g)

#Usando graph.bfs para calcular as shortest paths e armazenando numa matriz
for (i in 1:gorder(g)){
  shortpathv <-graph.bfs(g, root=i, "out",
                         order=TRUE, rank=TRUE, father=TRUE, pred=TRUE,
                         succ=TRUE, dist=TRUE)
  
  if ( i == 1) {
    distMatrix = as.matrix(shortpathv$dist)
  } else {
    distMatrix = cbind(distMatrix, shortpathv$dist)
  }
}

#Calculando a closeness centrality
closecentrality <- as.matrix(1/rowSums(distMatrix))
