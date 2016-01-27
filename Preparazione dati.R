#' # Creazione di cluster dei comuni trentini (confini 2016)
#' 
#' Variabili considerate:
#' * Dimensione demografiche (solo famiglie)
#' * Indice di vecchiaia
#' * Percentuale di stranieri
#' * Iscritti in I' sezione APIA
#' * Percentuali addetti in ASIAUL sett. industriale e costruzioni
#' * Presenze turistiche in strutture alberghiere e complementari
#' 
#' 

library(RODBC)
library(dplyr)
library(tidyr)
source('password.R')

#' ## Elenco dei comuni ai confini 2016
trr <- sqlQuery(myConnection(), 'SELECT * FROM dati..trrcomat') 
odbcCloseAll()

#' ## Estrazione dei dati da SQL Server
conn <- myConnection()

popolazione <- sqlQuery(conn, 'SELECT anno, comu, SUM(compfam) valore
                    FROM dati..dmdfaco 
                    WHERE anno IN (2009,2014)
                    AND comu BETWEEN 1 AND 998
                    GROUP BY anno, comu' )

anziani <- sqlQuery(conn, 'SELECT anno, comu, SUM(popolazi) valore
                    FROM dati..dmdeta 
                    WHERE anno IN (2009,2014) AND eta >= 65
                    AND comu BETWEEN 1 AND 998
                    GROUP BY anno, comu' )

giovani <- sqlQuery(conn, 'SELECT anno, comu, SUM(popolazi) valore 
                    FROM dati..dmdeta
                    WHERE anno IN (2009,2014) AND eta <= 14
                    AND comu BETWEEN 1 AND 998
                    GROUP BY anno, comu' )

stranieri <- sqlQuery(conn, 'SELECT anno, comu, SUM(numero) valore 
                    FROM dati..dmdstreta 
                    WHERE anno IN (2009,2014)
                    AND comu BETWEEN 1 AND 998
                    GROUP BY anno, comu' )

agricoltura <- sqlQuery(conn, 'SELECT anno, comu, SUM(numero) valore 
                    FROM dati..agdapiagen
                    WHERE anno IN (2010,2014) AND sezione = 1
                    AND comu BETWEEN 1 AND 998
                    GROUP BY anno, comu' )

addettiInd <- sqlQuery(conn, 'SELECT anno, comu, SUM(add_ul) valore 
                    FROM dati..ecdasiaul
                    WHERE anno IN (2007,2012) AND Settore IN (1, 2)
                    AND comu BETWEEN 1 AND 998
                    GROUP BY anno, comu' )

presenze <- sqlQuery(conn, 'SELECT anno, comu, SUM(presenze) valore
                    FROM dati..tudmotot
                    WHERE anno IN (2009,2014) AND tuccompar IN (0, 1)
                    GROUP BY anno, comu')


#' altitudine non è stata ancora salvata in DB, leggo da csv
altitudine <- read.csv2('trdcgeo4.csv', 
                        header = T, 
                        stringsAsFactors = F, 
                        sep = ';', 
                        colClasses = c('integer', 
                                       'character', 
                                       'numeric', 
                                       'numeric'),
                        dec = ','
)

odbcCloseAll()


#' ## Trasformazioni ai nuovi confini dei dati assoluti
#' 
as.nuoviConfini <- function(x, year) {
    name <- paste0(deparse(substitute(x)), substr(year, 3, 4))
    x <- x %>%
        filter(anno == year) %>%
        left_join(trr, by = 'comu') %>%
        group_by(anno, comuat) %>%
        summarize(name = sum(valore, na.rm = T)) %>%
        ungroup() %>%
        select(2:3)
    colnames(x) <- c('comuat', name)
    return(x)
}

#' ### Dataset al 2009 (o primo anno)
popolazione09 <- as.nuoviConfini(popolazione, 2009)

anziani09 <- as.nuoviConfini(anziani, 2009)

giovani09 <-  as.nuoviConfini(giovani, 2009)

stranieri09 <-  as.nuoviConfini(stranieri, 2009)

agricoltura10 <- as.nuoviConfini(agricoltura, 2010)

addettiInd07 <- as.nuoviConfini(addettiInd, 2007)

presenze09 <- as.nuoviConfini(presenze, 2009)


#' ### Dataset al 2014 (o ultimo anno)
popolazione14 <- as.nuoviConfini(popolazione, 2014)

anziani14 <- as.nuoviConfini(anziani, 2014)

giovani14 <-  as.nuoviConfini(giovani, 2014)

stranieri14 <-  as.nuoviConfini(stranieri, 2014)

agricoltura14 <- as.nuoviConfini(agricoltura, 2014)

addettiInd12 <- as.nuoviConfini(addettiInd, 2012)

presenze14 <- as.nuoviConfini(presenze, 2014)


#' ### Costruzione data.frame contenente indicatori
#' Dal dataset elimino il comune di Trento (205) in quanto 
#' chiaro outlier che potrebbe dare problemi (hint: li da) nella 
#' standardizzazione delle variabili

ind <- data.frame(popolazione14) %>%
    full_join(anziani14, by = 'comuat') %>%
    full_join(giovani14, by = 'comuat') %>%
    full_join(stranieri14, by = 'comuat') %>%
    full_join(agricoltura14, by = 'comuat') %>%
    full_join(addettiInd12, by = 'comuat') %>%
    full_join(presenze14, by = 'comuat') %>%
    full_join(popolazione09, by = 'comuat') %>%
    full_join(anziani09, by = 'comuat') %>%
    full_join(giovani09, by = 'comuat') %>%
    full_join(stranieri09, by = 'comuat') %>%
    full_join(agricoltura10, by = 'comuat') %>%
    full_join(addettiInd07, by = 'comuat') %>%
    full_join(presenze09, by = 'comuat') %>%
    full_join(altitudine, by = c('comuat' = 'comu')) %>%
    mutate(indVec14 = anziani14 / giovani14 * 100, 
           indStra14 = stranieri14 / popolazione14 * 100,
           indAgri14 = agricoltura14 / popolazione14 * 100, 
           indInd12 = addettiInd12 / popolazione14 * 100,
           indTur14 = presenze14 / popolazione14 * 100,
           indVec09 = anziani09 / giovani09 * 100, 
           indStra09 = stranieri09 / popolazione09 * 100,
           indAgri10 = agricoltura10 / popolazione09 * 100, 
           indInd07 = addettiInd07 / popolazione09 * 100,
           indTur09 = presenze09 / popolazione09 * 100,
           tasVarPop = (popolazione14 - popolazione09) / popolazione09,
           tasVarVec = (indVec14-indVec09) / indVec09,
           tasVarStra = (indStra14-indStra09) / indStra09,
           tasVarAgri = (indAgri14- indAgri10) / indAgri10,
           tasVarInd = (indInd12 - indInd07) / indInd07
           #tasVarTur = (indTur14 - indTur09) / indTur09
    ) %>%
    select(comuat, 
           altcentr, 
           popolazione14, 
           starts_with('ind'), 
           starts_with('tasVar')
           ) %>%
    filter(comuat != 205)

ind[is.na(ind)] <- 0


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

