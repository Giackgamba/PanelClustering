source('Preparazione dati.R')

#' Standardizzazione data.frame
scaledInd <- scale(ind[, -1])
rownames(scaledInd) <- ind[, 1]


#' Matrice delle distanze
d <- dist(scaledInd, method = 'euclidean')
fit <- hclust(d, method="ward.D2") 
plot(fit)

group <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")


#' aggiungo il cluster a data.frame iniziale

indNuoviComuni <- data.frame(ind, group) %>%
    left_join(trr, by = c('comuat' = 'comu')) %>%
    select(comu = comuat, descriz, group, 2:9) %>%
    mutate(indVec = round(indVec,2), 
           indStra = round(indStra, 2), 
           indAgri = round(indAgri, 2), 
           indInd = round(indInd, 2),
           indTur = round(indTur, 2)) %>%
    arrange(group)

write.csv2(indNuoviComuni, 'clustersNuovi.csv', row.names = F)

scaledInd <- data.frame(group, scaledInd)

n <- scaledInd %>%
    group_by(group) %>%
    summarise(count=n())

avgInd <- scaledInd %>%
    group_by(group) %>%
    summarise_each(funs(mean)) %>%
    mutate(ampiezza = round(ampiezza, 2), 
           indVec = round(indVec, 2),
           indStra = round(indStra, 2), 
           indAgri = round(indAgri, 2), 
           indInd = round(indInd, 2), 
           indTur = round(indTur, 2)) %>%
    bind_cols(n['count'])


kable(avgInd)

#' ------------------------------------
kable(comuGroup)

#' ----------------------------------
kable(select(trr, comuat, descriz))
