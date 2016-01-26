source('Preparazione dati.R')

#' ## Standardizzazione
#' Standardizzazione data.frame contenente ultimo anno e variazioni, senza altitudine
scaledInd.Var <- scale(select(ind, 
                              3:8, 
                              14:18
)
)
rownames(scaledInd.Var) <- ind[, 1]


#' Standardizzazione data.frame contenente ultimo anno, senza altitudine
scaledInd.Last <- scale(select(ind, 
                               3:8
)
)

rownames(scaledInd.Last) <- ind[, 1]


#' Standardizzazione data.frame contenente ultimo anno e variazioni, con altitudine

scaledInd.Var.Alt <- scale(select(ind, 
                                  2:8, 
                                  14:18
)
)
rownames(scaledInd.Var.Alt) <- ind[, 1]


#' Standardizzazione data.frame contenente ultimo anno, con altitudine
scaledInd.Last.Alt <- scale(select(ind, 
                                   2:8
)
)
rownames(scaledInd.Last.Alt) <- ind[, 1]


#' ## Matrici delle distanze
#' 
#' Distanze utlimo anno e variazioni, plot
d.Var <- dist(scaledInd.Var, method = 'euclidean')
fit <- hclust(d.Var, method="ward.D2") 
plot(fit)

group.Var <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")


#' Distanze utlimo anno, plot
d.Last <- dist(scaledInd.Last, method = 'euclidean')
fit <- hclust(d.Last, method="ward.D2") 
plot(fit)

group.Last <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")


#' Distanze utlimo anno e variazioni, con altitudine, plot
d.Var.Alt <- dist(scaledInd.Var.Alt, method = 'euclidean')
fit <- hclust(d.Var.Alt, method="ward.D2") 
plot(fit)

group.Var.Alt <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")

#' Distanze utlimo anno, con altitudine, plot
d.Last.Alt <- dist(scaledInd.Last.Alt, method = 'euclidean')
fit <- hclust(d.Last.Alt, method="ward.D2") 
plot(fit)

group.Last.Alt <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")


#' aggiungo i cluster a data.frame iniziale

indNuoviComuni <- data.frame(ind, 
                             group.Var, 
                             group.Last,
                             group.Var.Alt, 
                             group.Last.Alt
                             ) %>%
    left_join(trr, by = c('comuat' = 'comu')) %>%
    select(comu = comuat, 
           descriz = descriz, 
           group.Var,
           group.Last,
           group.Var.Alt,
           group.Last.Alt,
           contains('14'), 
           contains('10'), 
           contains('12'), 
           contains('tasVar'),
           contains('centr')
    ) %>%
    arrange(comu)

#' crea csv
write.csv2(indNuoviComuni, 'clustersNuovi.csv', row.names = F)

#' Calcola la media per ogni cluster

n.Last <- indNuoviComuni %>%
    group_by(group.Last) %>%
    summarise(count=n())

n.Var <- indNuoviComuni %>%
    group_by(group.Var) %>%
    summarise(count=n())

n.Last.Alt <- indNuoviComuni %>%
    group_by(group.Last.Alt) %>%
    summarise(count=n())

n.Var.Alt <- indNuoviComuni %>%
    group_by(group.Var.Alt) %>%
    summarise(count=n())


avgInd.Last <- indNuoviComuni %>%
    select(-contains('descriz'), 
           -contains('group.Var'),
           -contains('comu')
    ) %>%
    group_by(group.Last) %>%
    summarise_each(funs(mean)) %>%
    bind_cols(n.Last['count'])

avgInd.Var <- indNuoviComuni %>%
    select(-contains('descriz'), 
           -contains('group.Last'),
           -contains('comu')
    ) %>%
    group_by(group.Var) %>%
    summarise_each(funs(mean)) %>%
    bind_cols(n.Var['count'])

avgInd.Last.Alt <- indNuoviComuni %>%
    select(-contains('descriz'), 
           -contains('group.Var'),
           -contains('comu')
    ) %>%
    group_by(group.Last.Alt) %>%
    summarise_each(funs(mean)) %>%
    bind_cols(n.Last.Alt['count'])

avgInd.Var.Alt <- indNuoviComuni %>%
    select(-contains('descriz'), 
           -contains('group.Last'),
           -contains('comu')
    ) %>%
    group_by(group.Var.Alt) %>%
    summarise_each(funs(mean)) %>%
    bind_cols(n.Var.Alt['count'])

#' -----------------------------------

kable(avgInd.Last)

kable(avgInd.Last.Alt)

#' --------------------------------

kable(avgInd.Var)

kable(avgInd.Var.Alt)

#' ------------------------------------

kable(select(trr, comuat, descriz))
