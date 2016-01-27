# 
In questa sezione si utilizza il metodo gerarchico per la creazione di
cluster a partire dai quattro insiemi di dati




```r
source('Preparazione dati.R')
```

Il file 'PreparazioneDati.R produce 5 dataset:
* ind
* scaledInd.Var
* scaledInd.Last
* scaledInd.Var.Alt
* scaledInd.Last.Alt
## Matrici delle distanze

Distanze utlimo anno e variazioni, plot


```r
d.Var <- dist(scaledInd.Var, method = 'euclidean')
fit <- hclust(d.Var, method="ward.D2") 
plot(fit)

group.Var <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

Distanze utlimo anno, plot


```r
d.Last <- dist(scaledInd.Last, method = 'euclidean')
fit <- hclust(d.Last, method="ward.D2") 
plot(fit)

group.Last <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Distanze utlimo anno e variazioni, con altitudine, plot


```r
d.Var.Alt <- dist(scaledInd.Var.Alt, method = 'euclidean')
fit <- hclust(d.Var.Alt, method="ward.D2") 
plot(fit)

group.Var.Alt <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Distanze utlimo anno, con altitudine, plot


```r
d.Last.Alt <- dist(scaledInd.Last.Alt, method = 'euclidean')
fit <- hclust(d.Last.Alt, method="ward.D2") 
plot(fit)

group.Last.Alt <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

aggiungo i cluster a data.frame iniziale


```r
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
```

crea csv per uso in QGis


```r
write.csv2(indNuoviComuni, 'clustersNuovi.csv', row.names = F)
```

Calcola il numero di comuni per ogni cluster

Funziona per calcolare medie, numerosit√† di comuni e numerosit√† di popolazione


```r
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
        inner_join(avg, by = 'group') %>%
        mutate_each(funs(myfuns = round(., 2)), -group, -count, -population)
    
    return(res)
}


avgInd.Last <- sommario('Last')
avgInd.Last.Alt <- sommario('Last.Alt')
avgInd.Var <- sommario('Var')
avgInd.Var.Alt <- sommario('Var.Alt')
```

-----------------------------------


```r
kable(avgInd.Last)
```



| group| count| population| altcentr| popolazione14| indVec14| indStra14| indAgri14| indInd12| indTur14| indAgri10| tasVarPop| tasVarVec| tasVarStra| tasVarAgri| tasVarInd|
|-----:|-----:|----------:|--------:|-------------:|--------:|---------:|---------:|--------:|--------:|---------:|---------:|---------:|----------:|----------:|---------:|
|     1|    49|     133170|   587.24|       2717.76|   127.79|     11.20|      0.69|    14.49|  1999.08|      0.76|      0.03|      0.10|       0.10|      -0.12|     -0.12|
|     2|    88|     137036|   741.00|       1557.23|   166.21|      4.31|      0.92|     8.09|  2381.52|      1.00|      0.00|      0.12|       0.12|        Inf|     -0.09|
|     3|    13|      20552|  1084.69|       1580.92|   124.62|      7.26|      0.45|     7.66| 32053.95|      0.47|      0.03|      0.18|       0.31|      -0.11|     -0.05|
|     4|     4|      93148|   212.50|      23287.00|   132.63|     11.14|      0.25|    12.66|  3181.21|      0.30|      0.05|      0.04|       0.11|      -0.13|     -0.17|
|     5|    23|      32583|   653.70|       1416.65|   148.49|      9.25|      4.61|     6.98|   630.50|      4.80|     -0.01|      0.14|       0.04|      -0.05|     -0.17|

```r
kable(avgInd.Last.Alt)
```



| group| count| population| altcentr| popolazione14| indVec14| indStra14| indAgri14| indInd12| indTur14| indAgri10| tasVarPop| tasVarVec| tasVarStra| tasVarAgri| tasVarInd|
|-----:|-----:|----------:|--------:|-------------:|--------:|---------:|---------:|--------:|--------:|---------:|---------:|---------:|----------:|----------:|---------:|
|     1|    73|     180278|   527.51|       2469.56|   128.57|      9.25|      0.87|    13.09|  1477.00|      0.95|      0.03|      0.10|       0.05|      -0.09|     -0.13|
|     2|    13|      20552|  1084.69|       1580.92|   124.62|      7.26|      0.45|     7.66| 32053.95|      0.47|      0.03|      0.18|       0.31|      -0.11|     -0.05|
|     3|     4|      93148|   212.50|      23287.00|   132.63|     11.14|      0.25|    12.66|  3181.21|      0.30|      0.05|      0.04|       0.11|      -0.13|     -0.17|
|     4|    66|      93316|   868.73|       1413.88|   179.44|      4.47|      0.76|     7.66|  3114.52|      0.82|     -0.01|      0.13|       0.19|        Inf|     -0.05|
|     5|    21|      29195|   627.33|       1390.24|   146.45|      8.14|      5.11|     5.80|   411.94|      5.37|      0.00|      0.12|       0.00|      -0.05|     -0.20|

--------------------------------


```r
kable(avgInd.Var)
```



| group| count| population| altcentr| popolazione14| indVec14| indStra14| indAgri14| indInd12| indTur14| indAgri10| tasVarPop| tasVarVec| tasVarStra| tasVarAgri| tasVarInd|
|-----:|-----:|----------:|--------:|-------------:|--------:|---------:|---------:|--------:|--------:|---------:|---------:|---------:|----------:|----------:|---------:|
|     1|    37|     192318|   489.92|       5197.78|   126.24|     12.57|      1.06|    14.95|  2662.45|      1.16|      0.03|      0.05|       0.09|      -0.18|     -0.17|
|     2|   111|     192229|   741.79|       1731.79|   149.68|      5.84|      0.82|     9.08|  3743.82|      0.89|      0.01|      0.13|       0.06|        Inf|     -0.12|
|     3|     8|       5475|  1069.00|        684.38|   166.05|      5.87|      0.44|     5.51| 27422.60|      0.51|      0.01|      0.14|       1.53|      -0.11|     -0.03|
|     4|    15|      23430|   634.80|       1562.00|   145.64|      6.64|      5.23|     4.95|   463.92|      5.48|     -0.01|      0.14|      -0.10|      -0.04|     -0.31|
|     5|     6|       3037|   904.83|        506.17|   275.96|      3.42|      2.59|     8.92|  1840.36|      2.46|     -0.04|      0.16|       0.00|       0.27|      0.93|

```r
kable(avgInd.Var.Alt)
```



| group| count| population| altcentr| popolazione14| indVec14| indStra14| indAgri14| indInd12| indTur14| indAgri10| tasVarPop| tasVarVec| tasVarStra| tasVarAgri| tasVarInd|
|-----:|-----:|----------:|--------:|-------------:|--------:|---------:|---------:|--------:|--------:|---------:|---------:|---------:|----------:|----------:|---------:|
|     1|    50|     237596|   389.66|       4751.92|   118.16|     10.17|      0.82|    13.73|  2141.77|      0.90|      0.04|      0.06|       0.05|      -0.14|     -0.18|
|     2|    20|      21655|  1108.55|       1082.75|   148.00|      6.44|      0.43|     7.61| 20567.67|      0.49|      0.04|      0.16|       0.85|      -0.13|     -0.03|
|     3|    85|     128275|   792.73|       1509.12|   159.78|      5.62|      0.96|     8.67|  2521.18|      1.05|     -0.01|      0.14|       0.01|        Inf|     -0.12|
|     4|    16|      25926|   594.00|       1620.38|   146.81|      9.00|      5.07|     6.45|   479.47|      5.25|     -0.01|      0.12|       0.02|      -0.04|     -0.30|
|     5|     6|       3037|   904.83|        506.17|   275.96|      3.42|      2.59|     8.92|  1840.36|      2.46|     -0.04|      0.16|       0.00|       0.27|      0.93|

------------------------------------


```r
kable(select(trr, comuat, descriz))
```



| comuat|descriz                            |
|------:|:----------------------------------|
|      0|NON DISPONIBILE                    |
|      1|Ala                                |
|      2|Albiano                            |
|      3|Aldeno                             |
|    237|Amblar-Don                         |
|      5|Andalo                             |
|      6|Arco                               |
|      7|Avio                               |
|      9|Baselga di Pine'                   |
|     11|Bedollo                            |
|    232|Valdaone                           |
|     13|Besenello                          |
|    229|Ledro                              |
|     15|Bieno                              |
|    228|Comano Terme                       |
|     17|Bleggio Superiore                  |
|     18|Bocenago                           |
|    239|Borgo Lares                        |
|    246|Sella Giudicarie                   |
|     21|Bondone                            |
|     22|Borgo Valsugana                    |
|    236|Altopiano della Vigolana           |
|    246|Sella Giudicarie                   |
|     25|Brentonico                         |
|     26|Bresimo                            |
|     27|Brez                               |
|    238|Borgo Chiese                       |
|     29|Caderzone Terme                    |
|     30|Cagno'                             |
|    243|Madruzzo                           |
|     32|Calceranica al Lago                |
|     33|Caldes                             |
|     34|Caldonazzo                         |
|     35|Calliano                           |
|     36|Campitello di Fassa-Ciampedel      |
|     37|Campodenno                         |
|     38|Canal San Bovo                     |
|     39|Canazei-Cianacei                   |
|     40|Capriana                           |
|     41|Carano                             |
|     42|Carisolo                           |
|     43|Carzano                            |
|     45|Castel Condino                     |
|     46|Castelfondo                        |
|     47|Castello-Molina di Fiemme          |
|     48|Castello Tesino                    |
|     49|Castelnuovo                        |
|     50|Cavalese                           |
|     51|Cavareno                           |
|     52|Cavedago                           |
|     53|Cavedine                           |
|     54|Cavizzana                          |
|    241|Cembra Lisignago                   |
|    236|Altopiano della Vigolana           |
|    238|Borgo Chiese                       |
|     58|Cimone                             |
|     59|Cinte Tesino                       |
|     60|Cis                                |
|     61|Civezzano                          |
|     62|Cles                               |
|     63|Cloz                               |
|     64|Commezzadura                       |
|    229|Ledro                              |
|    238|Borgo Chiese                       |
|    230|Predaia                            |
|     68|Croviana                           |
|    242|Cont‡                              |
|     70|Daiano                             |
|     71|Dambel                             |
|    232|Valdaone                           |
|    244|Porte di Rendena                   |
|     74|Denno                              |
|    233|Dimaro Folgarida                   |
|    237|Amblar-Don                         |
|    231|San Lorenzo Dorsino                |
|     78|Drena                              |
|     79|Dro                                |
|     80|Faedo                              |
|     81|Fai della Paganella                |
|    235|Altavalle                          |
|     83|Fiave'                             |
|    245|Primiero San Martino di Castrozza  |
|     85|Fierozzo-Vlar˜tz                   |
|    242|Cont‡                              |
|     87|Folgaria                           |
|     88|Fondo                              |
|     89|Fornace                            |
|     90|Frassilongo-Garait                 |
|     91|Garniga Terme                      |
|     92|Giovo                              |
|     93|Giustino                           |
|    235|Altavalle                          |
|     95|Grigno                             |
|    235|Altavalle                          |
|     97|Imer                               |
|     98|Isera                              |
|     99|Ivano-Fracena                      |
|    246|Sella Giudicarie                   |
|    243|Madruzzo                           |
|    102|Lavarone                           |
|    103|Lavis                              |
|    104|Levico Terme                       |
|    241|Cembra Lisignago                   |
|    106|Livo                               |
|    228|Comano Terme                       |
|    108|Lona-Lases                         |
|    109|Luserna-Lus⁄rn                     |
|    110|Male'                              |
|    111|Malosco                            |
|    112|Massimeno                          |
|    113|Mazzin-Mazin                       |
|    114|Mezzana                            |
|    115|Mezzano                            |
|    116|Mezzocorona                        |
|    117|Mezzolombardo                      |
|    118|Moena-Moena                        |
|    229|Ledro                              |
|    120|Molveno                            |
|    233|Dimaro Folgarida                   |
|    247|Tre Ville                          |
|    123|Mori                               |
|    124|Nago-Torbole                       |
|    249|Ville d'Anaunia                    |
|    126|Nave San Rocco                     |
|    127|Nogaredo                           |
|    128|Nomi                               |
|    129|Novaledo                           |
|    130|Ospedaletto                        |
|    131|Ossana                             |
|    248|Vallelaghi                         |
|    133|Pal® del Fersina-Palai en Bersntol |
|    134|Panchia'                           |
|    135|Ronzo-Chienis                      |
|    136|Peio                               |
|    137|Pellizzano                         |
|    138|Pelugo                             |
|    139|Pergine Valsugana                  |
|    234|Pieve di Bono-Prezzo               |
|    229|Ledro                              |
|    142|Pieve Tesino                       |
|    143|Pinzolo                            |
|    144|Pomarolo                           |
|    145|Pozza di Fassa-Poza                |
|    232|Valdaone                           |
|    147|Predazzo                           |
|    247|Tre Ville                          |
|    234|Pieve di Bono-Prezzo               |
|    150|Rabbi                              |
|    247|Tre Ville                          |
|    152|Revo'                              |
|    153|Riva del Garda                     |
|    154|Romallo                            |
|    155|Romeno                             |
|    156|Roncegno Terme                     |
|    157|Ronchi Valsugana                   |
|    246|Sella Giudicarie                   |
|    159|Ronzone                            |
|    160|Rovere' della Luna                 |
|    161|Rovereto                           |
|    162|Ruffre'-Mendola                    |
|    163|Rumo                               |
|    164|Sagron Mis                         |
|    165|Samone                             |
|    231|San Lorenzo Dorsino                |
|    167|San Michele all'Adige              |
|    168|Sant'Orsola Terme                  |
|    169|Sanzeno                            |
|    170|Sarnonico                          |
|    171|Scurelle                           |
|    172|Segonzano                          |
|    173|Sfruz                              |
|    245|Primiero San Martino di Castrozza  |
|    230|Predaia                            |
|    176|Soraga-Soraga                      |
|    177|Sover                              |
|    240|Castel Ivano                       |
|    179|Spiazzo                            |
|    180|Spormaggiore                       |
|    181|Sporminore                         |
|    182|Stenico                            |
|    183|Storo                              |
|    184|Strembo                            |
|    240|Castel Ivano                       |
|    230|Predaia                            |
|    249|Ville d'Anaunia                    |
|    188|Telve                              |
|    189|Telve di Sopra                     |
|    190|Tenna                              |
|    191|Tenno                              |
|    248|Vallelaghi                         |
|    193|Terragnolo                         |
|    242|Cont‡                              |
|    195|Terzolas                           |
|    196|Tesero                             |
|    229|Ledro                              |
|    229|Ledro                              |
|    199|Tione di Trento                    |
|    200|Ton                                |
|    245|Primiero San Martino di Castrozza  |
|    202|Torcegno                           |
|    203|Trambileno                         |
|    245|Primiero San Martino di Castrozza  |
|    205|Trento                             |
|    230|Predaia                            |
|    249|Ville d'Anaunia                    |
|    235|Altavalle                          |
|    209|Valfloriana                        |
|    210|Vallarsa                           |
|    211|Varena                             |
|    236|Altopiano della Vigolana           |
|    213|Vermiglio                          |
|    230|Predaia                            |
|    248|Vallelaghi                         |
|    216|Vignola-Falesina                   |
|    217|Vigo di Fassa-Vich                 |
|    236|Altopiano della Vigolana           |
|    244|Porte di Rendena                   |
|    240|Castel Ivano                       |
|    222|Villa Lagarina                     |
|    244|Porte di Rendena                   |
|    224|Volano                             |
|    225|Zambana                            |
|    226|Ziano di Fiemme                    |
|    239|Borgo Lares                        |
|    228|Comano Terme                       |
|    229|Ledro                              |
|    230|Predaia                            |
|    231|San Lorenzo Dorsino                |
|    232|Valdaone                           |
|    233|Dimaro Folgarida                   |
|    234|Pieve di Bono-Prezzo               |
|    235|Altavalle                          |
|    236|Altopiano della Vigolana           |
|    237|Amblar-Don                         |
|    238|Borgo Chiese                       |
|    239|Borgo Lares                        |
|    240|Castel Ivano                       |
|    241|Cembra Lisignago                   |
|    242|Cont‡                              |
|    243|Madruzzo                           |
|    244|Porte di Rendena                   |
|    245|Primiero San Martino di Castrozza  |
|    246|Sella Giudicarie                   |
|    247|Tre Ville                          |
|    248|Vallelaghi                         |
|    249|Ville d'Anaunia                    |
|    999|FUORI PROVINCIA                    |

