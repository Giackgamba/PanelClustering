source('Preparazione dati.R')


#' # Analisi PCA

ind_pca <- prcomp(as.data.frame(Ind.Last), scale = T)

comp <- as.data.frame(ind_pca$x[,1:4])

#' ## K-Medie

#' calcola i cluster

k <- kmeans(comp, 5, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)


library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust, type = 's', size = 1)

#' Crea csv per uso in QGis
clusters_pca <- data.frame(comu = ind[, 1], Ind.Last, k$cluster)
write.csv2(clusters_pca, 'clusters_pca.csv', row.names = F)

n.K <- clusters_pca %>%
    group_by(k.cluster) %>%
    summarize(count = n())
avgs <- clusters_pca %>%
    group_by(k.cluster) %>%
    summarize_each(funs(mean))

avgs.K <- full_join(avgs, n.K)

#' ## Hierarchical

d.PCA <- dist(comp, method = 'manhattan')
fit <- hclust(d.PCA, method="ward.D2") 
plot(fit)

group.Last <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")

#' Crea csv per uso in QGis
cluster_pca_hier <- data.frame(comu = ind[, 1], Ind.Last, group.Last)
write.csv2(cluster_pca_hier, 'clustersPCAHier.csv', row.names = F)

n.H <- cluster_pca_hier %>%
    group_by(group.Last) %>%
    summarize(count = n())
avgs <- cluster_pca_hier %>%
    group_by(group.Last) %>%
    summarize_each(funs(mean))

avgs.H <- full_join(avgs, n.H)


population <- cluster_pca_hier %>%
    group_by(group.Last) %>%
    summarise(population = sum(popolazione14))

kable(avgs.H)
kable(population)