#' # 
#' In questa sezione si utilizza il metodo gerarchico per la creazione di
#' cluster a partire dai quattro insiemi di dati
#' 
#' 
source('Preparazione dati.R')

#' Il file 'PreparazioneDati.R produce 5 dataset:
#' * ind
#' * scaledInd.Var
#' * scaledInd.Last
#' * scaledInd.Var.Alt
#' * scaledInd.Last.Alt


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
           matches('14|10|12|tasVar|centr')
    ) %>%
    arrange(comu)

#' crea csv per uso in QGis
write.csv2(indNuoviComuni, 'clustersNuovi.csv', row.names = F)

#' Calcola il numero di comuni per ogni cluster
#' 

#' Funziona per calcolare medie, numerosità di comuni e numerosità di popolazione
sommario <- function(type) {
    var_name <- as.symbol(paste0('group.', type))
    
    n <- indNuoviComuni %>%
        group_by(group = eval(var_name)) %>%
        summarise(count = n())
    
    avg <- indNuoviComuni %>%
        select(group = matches(
            paste0(as.character(var_name), '$')
        ),
        7:19) %>%
        group_by(group) %>%
        summarise_each(funs(mean))
    
    pop <- indNuoviComuni %>%
        group_by(group = eval(var_name)) %>%
        summarise(population = sum(popolazione14))
    
    res <- inner_join(n, pop, by = 'group') %>%
        inner_join(avg, by = 'group')
    
    return(res)
}


avgInd.Last <- sommario('Last')
avgInd.Last.Alt <- sommario('Last.Alt')
avgInd.Var <- sommario('Var')
avgInd.Var.Alt <- sommario('Var.Alt')

#' -----------------------------------

kable(avgInd.Last)

kable(avgInd.Last.Alt)

#' --------------------------------

kable(avgInd.Var)

kable(avgInd.Var.Alt)

#' ------------------------------------

kable(select(trr, comuat, descriz))


