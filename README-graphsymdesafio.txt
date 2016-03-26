Foi utlizado o biblioteca igraph do R para gerar o grafo a partir
da lista de edges que foi fornecido.

Uma vez gerado o grafo "g" foi utlizado o algoritmo BFS da biblioteca igraph
para calcular os caminhos curtos (shortest paths ) das vértices que foram
armazenados em uma matriz "distMatrix".

Depois foi criada uma nova matriz chamada "closecentrality" que armazena as
closeness centralities de todas a vertices do matriz, a formula utilizada 
segundo segundo link fornecido no enunciado do desafio.
