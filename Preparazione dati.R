#' # Creazione di cluster dei comuni trentini (confini 2016)
#' 
#' Variabili considerate:
#' * Dimensione demografiche (solo famiglie)
#' * Indice di vecchiaia (solo famiglie)
#' * Percentuale di stranieri
#' * Iscritti in I° sezione APIA
#' * Percentuali addetti in ASIAUL sett. industriale e costruzioni
#' * Presenze turistiche
#' 
#' 

library(RODBC)
library(dplyr)

#+ results='hide'
usr <- 'sa'
psw <- 'Trento1921'

conn <- odbcConnect('produzione.dati', usr, psw)

trrSoppressi <- read.csv2('comuni soppressi.csv', sep = ',', 
                          header = T, stringsAsFactors = F) %>%
    select(comu = comune, comuat = nuovo.codice, 
           descriz = denominazione) 

trrNuovi <- read.csv2('comuni nuovi.csv', sep = ',', 
                      header = T, stringsAsFactors = F) %>% 
    select(comu = comune, descriz = denominazione) %>%
    mutate(comuat = comu)

trr <- sqlQuery(conn, 'SELECT * FROM dati..trrcomat') 
# %>%
#     full_join(trrSoppressi, by = 'comu') %>%
#     mutate(comuat = ifelse(
#         is.na(comuat.y), yes = comuat.x, no = comuat.y)
#         ) %>%
#     mutate(descriz = descriz.x) %>%
#     select(comu, comuat, descriz) %>%
#     bind_rows(trrNuovi) %>%
#     filter(comu %in% 1:998) %>%
#     distinct() %>%
#     arrange(comu)

odbcCloseAll()
rm(trrSoppressi)
rm(trrNuovi)


#' Estrazione dei dati da SQL Server
conn <- odbcConnect('produzione.dati', usr, psw)

ampiezza <- sqlQuery(conn, 'SELECT comu, SUM(compfam) valore
                    FROM dati..dmdfaco 
                    WHERE anno = 2014
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

anziani <- sqlQuery(conn, 'SELECT comu, SUM(popolazi) valore
                    FROM dati..dmdeta 
                    WHERE anno = 2014 AND eta >= 65
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

giovani <- sqlQuery(conn, 'SELECT comu, SUM(popolazi) valore 
                    FROM dati..dmdeta
                    WHERE anno = 2014 AND eta <= 14
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

stranieri <- sqlQuery(conn, 'SELECT comu, SUM(numero) valore 
                    FROM dati..dmdstreta 
                    WHERE anno = 2014
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

agricoltori <- sqlQuery(conn, 'SELECT comu, SUM(numero) valore 
                    FROM dati..agdapiacle
                    WHERE anno = 2014 AND sezione = 1
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

addettiInd <- sqlQuery(conn, 'SELECT comu, SUM(add_ul) valore 
                    FROM dati..ecdasiaul
                    WHERE anno = 2012 AND Settore = 1
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

addettiCostr <- sqlQuery(conn, 'SELECT comu, SUM(add_ul) valore 
                    FROM dati..ecdasiaul
                    WHERE anno = 2012 AND Settore = 2
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

addettiComTur <- sqlQuery(conn, 'SELECT comu, SUM(add_ul) valore 
                    FROM dati..ecdasiaul
                    WHERE anno = 2012 AND Settore IN (3, 4)
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

addettiAltro <- sqlQuery(conn, 'SELECT comu, SUM(add_ul) valore 
                    FROM dati..ecdasiaul
                    WHERE anno = 2012 AND Settore IN (5, 6)
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

addettiTot <- sqlQuery(conn, 'SELECT comu, SUM(add_ul) valore 
                    FROM dati..ecdasiaul
                    WHERE anno = 2012
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

laureati <- sqlQuery(conn, 'SELECT comu, SUM(popolazi) valore
                    FROM dati..dmd1tits
                    WHERE dmc1tits >= 7
                    GROUP BY comu' )

diplomati <- sqlQuery(conn, 'SELECT comu, SUM(popolazi) valore
                    FROM dati..dmd1tits
                    WHERE dmc1tits BETWEEN 5 AND 6
                    GROUP BY comu' )

presenze <- sqlQuery(conn, 'SELECT comu, SUM(presenze)/3 valore
                    FROM dati..tudmotot
                    WHERE anno BETWEEN 2012 AND 2014
                    GROUP BY comu')

popMedia <- sqlQuery(conn, 'SELECT comu, SUM(compfam)/3 valore
                    FROM dati..dmdfaco 
                    WHERE anno BETWEEN 2012 AND 2014
                    AND comu BETWEEN 1 AND 998
                    GROUP BY comu' )

altitudine <- sqlQuery(conn, 'SELECT altcentr FROM dati..trdcgeo3')

odbcCloseAll()


#' Trasformazioni ai nuovi confini dei dati assoluti
#' 
as.nuoviConfini <- function(x) {
    name <- deparse(substitute(x))
    x <- x %>%
        full_join(trr, by = 'comu') %>%
        group_by(comuat) %>%
        summarize(name = sum(valore, na.rm = T)) %>%
        ungroup() 
    colnames(x) <- c('comuat', name)
    return(x)
}

ampiezza <- as.nuoviConfini(ampiezza)

anziani <- as.nuoviConfini(anziani)

giovani <-  as.nuoviConfini(giovani)

stranieri <-  as.nuoviConfini(stranieri)

agricoltori <- as.nuoviConfini(agricoltori)

addettiInd <- as.nuoviConfini(addettiInd)

addettiCostr <- as.nuoviConfini(addettiCostr)

# addettiComTur <- as.nuoviConfini(addettiComTur)
# 
# addettiAltro <- as.nuoviConfini(addettiAltro)
# 
# addettiTot <- as.nuoviConfini(addettiTot)
# 
# laureati <- as.nuoviConfini(laureati)
# 
# diplomati <- as.nuoviConfini(diplomati)

presenze <- as.nuoviConfini(presenze)

popMedia <- as.nuoviConfini(popMedia)

#altitudine <- as.nuoviConfini(altitudine)

#' Costruzione data.frame contenente indicatori
#' 

ind <- data.frame(ampiezza) %>%
    full_join(anziani, by = 'comuat') %>%
    full_join(giovani, by = 'comuat') %>%
    full_join(stranieri, by = 'comuat') %>%
    full_join(agricoltori, by = 'comuat') %>%
    full_join(addettiInd, by = 'comuat') %>%
    full_join(addettiCostr, by = 'comuat') %>%
    full_join(presenze, by = 'comuat') %>%
    full_join(popMedia, by = 'comuat') %>%
#     full_join(addettiComTur, by = 'comuat') %>%
#     full_join(addettiAltro, by = 'comuat') %>%
#     full_join(addettiTot, by = 'comuat') %>%
#     full_join(laureati, by = 'comuat') %>%
#     full_join(diplomati, by = 'comuat') %>%
    mutate(indVec = anziani / giovani * 100, 
           indStra = stranieri / ampiezza * 100,
           indAgri = agricoltori / ampiezza * 100, 
           indInd = (addettiInd + addettiCostr) / ampiezza * 100,
           indTur = presenze / popMedia * 100
#           ,
#           indAlt = ifelse(altitudine>600, 'alto', 'basso')
#           indCostr = addettiCostr / addettiTot * 100,
#           indComTur = addettiComTur / addettiTot * 100,
#           indAltro = addettiAltro / addettiTot * 100,
#            indLau = laureati / ampiezza * 100,
#            indDip = diplomati / ampiezza * 100
            
           ) %>%
    select(comuat, ampiezza, starts_with('ind')) %>%
    filter(comuat != 205) %>%
    na.omit()


