

```r
source('Preparazione dati.R')
```

# Analisi PCA


```r
ind_pca <- prcomp(as.data.frame(Ind.Last), scale = T)

comp <- as.data.frame(ind_pca$x[,1:4])
```

## K-Medie
calcola i cluster


```r
k <- kmeans(comp, 5, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust, type = 's', size = 1)
```

Crea csv per uso in QGis


```r
clusters_pca <- data.frame(comu = ind[, 1], Ind.Last, k$cluster)
write.csv2(clusters_pca, 'clusters_pca.csv', row.names = F)

n.K <- clusters_pca %>%
    group_by(k.cluster) %>%
    summarize(count = n())
avgs <- clusters_pca %>%
    group_by(k.cluster) %>%
    summarize_each(funs(mean))

avgs.K <- full_join(avgs, n.K)
```

```
## Joining by: "k.cluster"
```

## Hierarchical


```r
d.PCA <- dist(comp, method = 'manhattan')
fit <- hclust(d.PCA, method="ward.D2") 
plot(fit)

group.Last <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Crea csv per uso in QGis


```r
cluster_pca_hier <- data.frame(comu = ind[, 1], Ind.Last, group.Last)
write.csv2(cluster_pca_hier, 'clustersPCAHier.csv', row.names = F)

n.H <- cluster_pca_hier %>%
    group_by(group.Last) %>%
    summarize(count = n())
avgs <- cluster_pca_hier %>%
    group_by(group.Last) %>%
    summarize_each(funs(mean))

avgs.H <- full_join(avgs, n.H)
```

```
## Joining by: "group.Last"
```

```r
population <- cluster_pca_hier %>%
    group_by(group.Last) %>%
    summarise(population = sum(popolazione14))

kable(avgs.H)
```



| group.Last|      comu| popolazione14| indVec14| indStra14| indAgri14|  indInd12|   indTur14| count|
|----------:|---------:|-------------:|--------:|---------:|---------:|---------:|----------:|-----:|
|          1|  96.73333|     10325.667| 130.9415| 10.575437| 0.5416067| 22.432897|  1476.6400|    15|
|          2| 125.57317|      1601.329| 172.2341|  4.719327| 0.9836342|  6.535833|  2314.8619|    82|
|          3| 130.41667|      1666.417| 125.6154|  7.320474| 0.4557681|  7.463592| 33368.6678|    12|
|          4| 136.50943|      1724.811| 126.5463|  9.931030| 0.9698461| 13.284251|  2502.4432|    53|
|          5| 118.46667|      1258.867| 143.5070|  8.005288| 5.4883868|  4.434331|   428.9388|    15|

```r
kable(population)
```



| group.Last| population|
|----------:|----------:|
|          1|     154885|
|          2|     131309|
|          3|      19997|
|          4|      91415|
|          5|      18883|

