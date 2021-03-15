# Point Cloud Geometry Compression Dyadic Decomposition

- O projeto se trata de um algoritmo de compressão geométrica 
de um Point Cloud 3D baseado nos papers publicado pelos professores Edil Medeiros e Eduardo Peixoto, técnica de compressão baseada na *dyadic decomposition*. Sua estrutura consiste em 3 módulos:

1. Um parser, que lerá um arquivo `.ply` e será capaz de construir a point cloud
 para o programa processá-lo(já feito e concedido pelo professor).

1. O algoritmo baseado na Dyadic decomposition, assim como as estruturas de dados 
necessários para o seu funcionamento(PointCloud, Pixel, Silhouette, árvore de 
decomposição...)

1. Codificação Aritmético binário com contexto adaptativo(CABAC), que será responsável 
 por codificar a compressão geométrica do Point Cloud para o arquivo comprimido final.

1. Um writer de Bitstream do arquivo .ply comprimido pelo algoritmo proposto.
