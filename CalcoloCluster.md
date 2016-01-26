

```r
source('Preparazione dati.R')
```

## Standardizzazione
Standardizzazione data.frame contenente ultimo anno e variazioni, senza altitudine


```r
scaledInd.Var <- scale(select(ind, 
                              3:8, 
                              14:18
)
)
rownames(scaledInd.Var) <- ind[, 1]
```

Standardizzazione data.frame contenente ultimo anno, senza altitudine


```r
scaledInd.Last <- scale(select(ind, 
                               3:8
)
)

rownames(scaledInd.Last) <- ind[, 1]
```

Standardizzazione data.frame contenente ultimo anno e variazioni, con altitudine


```r
scaledInd.Var.Alt <- scale(select(ind, 
                                  2:8, 
                                  14:18
)
)
rownames(scaledInd.Var.Alt) <- ind[, 1]
```

Standardizzazione data.frame contenente ultimo anno, con altitudine


```r
scaledInd.Last.Alt <- scale(select(ind, 
                                   2:8
)
)
rownames(scaledInd.Last.Alt) <- ind[, 1]
```

## Matrici delle distanze

Distanze utlimo anno e variazioni, plot


```r
d.Var <- dist(scaledInd.Var, method = 'euclidean')
fit <- hclust(d.Var, method="ward.D2") 
plot(fit)

group.Var <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Distanze utlimo anno, plot


```r
d.Last <- dist(scaledInd.Last, method = 'euclidean')
fit <- hclust(d.Last, method="ward.D2") 
plot(fit)

group.Last <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

Distanze utlimo anno e variazioni, con altitudine, plot


```r
d.Var.Alt <- dist(scaledInd.Var.Alt, method = 'euclidean')
fit <- hclust(d.Var.Alt, method="ward.D2") 
plot(fit)

group.Var.Alt <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Distanze utlimo anno, con altitudine, plot


```r
d.Last.Alt <- dist(scaledInd.Last.Alt, method = 'euclidean')
fit <- hclust(d.Last.Alt, method="ward.D2") 
plot(fit)

group.Last.Alt <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

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
           contains('14'), 
           contains('10'), 
           contains('12'), 
           contains('tasVar'),
           contains('centr')
    ) %>%
    arrange(comu)
```

crea csv


```r
write.csv2(indNuoviComuni, 'clustersNuovi.csv', row.names = F)
```

Calcola la media per ogni cluster


```r
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
```

-----------------------------------


```r
kable(avgInd.Last)
```



| group.Last| group.Last.Alt| popolazione14| indVec14| indStra14|  indTur14| indAgri10|  indInd12|  tasVarPop| tasVarVec| tasVarStra| tasVarAgri|  tasVarInd|  altcentr| count|
|----------:|--------------:|-------------:|--------:|---------:|---------:|---------:|---------:|----------:|---------:|----------:|----------:|----------:|---------:|-----:|
|          1|       1.533333|     2900.6000| 123.9985| 11.884489|  1031.354|  2457.572| 15.493974|  0.0233456| 0.0844122|  0.0493035| -0.0503923| -0.1320699|  526.5111|    45|
|          2|       2.944954|     1553.7523| 152.7792|  5.458783|  2383.715|  2935.272|  7.710476|  0.0065371| 0.1241347|  0.1192012| -0.0017274| -0.1111710|  724.8165|   109|
|          3|       4.384615|     1580.9231| 124.6187|  7.256321| 32053.947|  1391.647|  7.655651|  0.0332541| 0.1802398|  0.3117538| -0.0901079| -0.0499700| 1084.6923|    13|
|          4|       4.000000|    23287.0000| 132.6258| 11.140084|  3181.206|  1363.627| 12.664979|  0.0468635| 0.0424355|  0.1088444| -0.1915032| -0.1651305|  212.5000|     4|
|          5|       5.000000|      483.8333| 345.1429|  1.864021|  2632.307|  1946.337|  7.453518| -0.0573293| 0.1842213|  0.2386001| -0.2147412|  0.1091768| 1053.3333|     6|

```r
kable(avgInd.Last.Alt)
```



| group.Last.Alt| group.Last| popolazione14| indVec14| indStra14|   indTur14| indAgri10|  indInd12| tasVarPop| tasVarVec| tasVarStra| tasVarAgri|  tasVarInd|  altcentr| count|
|--------------:|----------:|-------------:|--------:|---------:|----------:|---------:|---------:|---------:|---------:|----------:|----------:|----------:|---------:|-----:|
|              1|   1.608696|      2413.043| 132.5012|  8.687450|  1036.1861|  3303.109|  9.519201| 0.0177131| 0.1064667|  0.0546212| -0.0596802| -0.1430576|  520.5435|    92|
|              2|   1.000000|      1504.000| 129.8615|  8.256360|   277.7686|  2279.953| 48.697707| 0.0222741| 0.0323682|  0.1477045|  0.0346430| -0.0184657|  452.7500|     4|
|              3|   3.000000|      1148.500| 131.3755|  5.626736| 52904.9082|  1131.770|  6.514696| 0.0149319| 0.1291452|  0.4733730| -0.2872725| -0.0122628| 1223.7500|     4|
|              4|   4.000000|     23287.000| 132.6258| 11.140084|  3181.2057|  1363.627| 12.664979| 0.0468635| 0.0424355|  0.1088444| -0.1915032| -0.1651305|  212.5000|     4|
|              5|   2.301370|      1242.890| 173.8179|  5.212985|  5899.6057|  1955.779|  8.017797| 0.0010000| 0.1415985|  0.1806373|  0.0217165| -0.0653579|  938.6712|    73|

--------------------------------


```r
kable(avgInd.Var)
```



| group.Var| group.Var.Alt| popolazione14| indVec14| indStra14|  indTur14| indAgri10|  indInd12|  tasVarPop| tasVarVec| tasVarStra| tasVarAgri|  tasVarInd|  altcentr| count|
|---------:|-------------:|-------------:|--------:|---------:|---------:|---------:|---------:|----------:|---------:|----------:|----------:|----------:|---------:|-----:|
|         1|      1.153846|     3824.3692| 123.6592| 10.615656|  3404.664|  2119.690| 13.584727|  0.0368447| 0.0659916|  0.1077875| -0.0609059| -0.1209070|  584.2923|    65|
|         2|      2.219780|     1624.6154| 160.6814|  5.270097|  1787.038|  2925.815|  7.469497| -0.0045966| 0.1338818|  0.0357701| -0.0629784| -0.1659868|  704.8571|    91|
|         3|      2.000000|     1130.3846| 142.7967|  6.404176| 26938.209|  1735.273|  7.134261|  0.0293298| 0.2329549|  0.8710949| -0.1618245| -0.0387095| 1100.4615|    13|
|         4|      4.000000|      506.1667| 275.9635|  3.417057|  1840.360|  4674.272|  8.917746| -0.0404507| 0.1628838|  0.0038553| -0.1637051|  0.9312624|  904.8333|     6|
|         5|      5.000000|     1166.5000| 141.1366|  3.266432|  3146.705|  5563.040| 11.795915| -0.0381582| 0.2345011| -0.2386331|  3.5470280| -0.4182268| 1056.5000|     2|

```r
kable(avgInd.Var.Alt)
```



| group.Var.Alt| group.Var| popolazione14| indVec14| indStra14|  indTur14| indAgri10|  indInd12|  tasVarPop| tasVarVec| tasVarStra| tasVarAgri|  tasVarInd|  altcentr| count|
|-------------:|---------:|-------------:|--------:|---------:|---------:|---------:|---------:|----------:|---------:|----------:|----------:|----------:|---------:|-----:|
|             1|  1.458716|      2307.147| 130.5067|  8.254187|  2219.591|  2659.207| 10.738575|  0.0254662| 0.0700870|  0.0641986| -0.0500374| -0.1402603|  614.5505|   109|
|             2|  2.523810|      1287.476| 156.7079|  5.858700| 21147.755|  1896.113|  6.834128|  0.0206020| 0.1967786|  0.6484879| -0.0912753| -0.0815526| 1118.2381|    21|
|             3|  1.000000|     23287.000| 132.6258| 11.140084|  3181.206|  1363.627| 12.664979|  0.0468635| 0.0424355|  0.1088444| -0.1915032| -0.1651305|  212.5000|     4|
|             4|  2.292683|      1036.390| 198.1803|  5.025700|  1131.693|  2914.800|  8.397611| -0.0312370| 0.2082158|  0.0135940| -0.1131460| -0.0053147|  744.8049|    41|
|             5|  5.000000|      1166.500| 141.1366|  3.266432|  3146.705|  5563.040| 11.795915| -0.0381582| 0.2345011| -0.2386331|  3.5470280| -0.4182268| 1056.5000|     2|

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
|    242|Contà                              |
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
|     85|Fierozzo-Vlar÷tz                   |
|    242|Contà                              |
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
|    109|Luserna-LusÚrn                     |
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
|    133|Pal¨ del Fersina-Palai en Bersntol |
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
|    242|Contà                              |
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
|    242|Contà                              |
|    243|Madruzzo                           |
|    244|Porte di Rendena                   |
|    245|Primiero San Martino di Castrozza  |
|    246|Sella Giudicarie                   |
|    247|Tre Ville                          |
|    248|Vallelaghi                         |
|    249|Ville d'Anaunia                    |
|    999|FUORI PROVINCIA                    |

