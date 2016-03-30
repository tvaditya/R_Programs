Foi utlizado o biblioteca igraph do R para gerar o grafo a partir
da lista de edges que foi fornecido.

Uma vez gerado o grafo "g" foi utlizado o algoritmo BFS da biblioteca igraph
para calcular os caminhos curtos (shortest paths ) das vértices que foram
armazenados em uma matriz "distMatrix".

Depois foi criada uma nova matriz chamada "closecentrality" que armazena as
closeness centralities de todas a vertices do matriz, a formula utilizada 
segundo segundo link fornecido no enunciado do desafio.

Uma vez calculada a closeness centrality foi visot que é o nó de maior centralidade 
era o 44 com o valor de 0.005988024

Após isso usamos  Freeman para calcular a centralided do grafo o valor deu
0.1096407
