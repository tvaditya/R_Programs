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

# Numero de vertices
n <- gorder(g)

#Usando graph.bfs para calcular as shortest paths e armazenando numa matriz
for (i in 1:n){
  shortpathv <-graph.bfs(g, root=i, "all",
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

#Centralidade do Grafo via Freeman, centdif é o somatorio das diferenças entre
#entre a vertice de maior centralidade em relacao as outras 
centdif <- 0
for ( i in 1:length(closecentrality)){
  soma <- max(closecentrality) - closecentrality[i]
  centdif <- centdif + soma
  
}
# O grafo maximo é uma estrela e sua centralidade equivale
# ao seguinte (n - 2) /(2*n - 3) onde n é o numero de vertice
maxcentral <- (n - 2) / (2*n - 3)

#Temos entao a centralidade do grafo por Freeman
centralidadegrafo <- centdif/maxcentral

#Calculando a centralidade do grafo baseado em closeness
centr_clo(g)
centr_betw(g)
centr_eigen(g)
