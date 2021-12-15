# Singapore Philosophy Group Belief Typology: code

This repository contains the R code used to produce the analysis and plots described in the 
[Singapore Philosophy Group Belief Typology writeup](https://sg-philosophy.notion.site/sg-philosophy/Singapore-Philosophy-Group-Belief-Typology-aa1fd693196b45118fc41f1419a6abe7). 

Different flavours of cluster analysis were performed (k-means, partition-around-medoids) on both unstandardized and standardized variables and the best combination selected based on the most distinct separation of resulting clusters via visual inspection. Optimal number of clusters was determined using the [silhouette](https://en.wikipedia.org/wiki/Silhouette_(clustering)) method as provided by the function `fviz_nbclust` in the `factoextra` package.

For Principal Components Analysis, function `prcomp` was used and variable plotting function `fviz_pca_var` was used on the results to visually inspect the loadings of each question onto pairs of orthogonal dimensions.