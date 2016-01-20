

```r
source('Preparazione dati.R')
```

Standardizzazione data.frame


```r
scaledInd <- scale(ind[, -1])
rownames(scaledInd) <- ind[, 1]
```

Matrice delle distanze


```r
d <- dist(scaledInd, method = 'maximum')
fit <- hclust(d, method="ward.D2") 
plot(fit)

group <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

aggiungo il cluster a data.frame iniziale


```r
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
           indVec = round(indVec,2),
           indStra = round(indStra, 2), 
           indAgri = round(indAgri, 2), 
           indInd = round(indInd, 2), 
           indTur = round(indTur, 2)) %>%
    bind_cols(n['count'])


kable(avgInd)
```



| group| ampiezza| indVec| indStra| indAgri| indInd| indTur| count|
|-----:|--------:|------:|-------:|-------:|------:|------:|-----:|
|     1|     0.04|  -0.36|    1.16|   -0.12|   0.20|  -0.24|    50|
|     2|     2.57|  -0.35|    0.63|   -0.55|   2.60|  -0.28|     8|
|     3|    -0.18|   0.32|   -0.69|   -0.29|  -0.19|  -0.19|    89|
|     4|    -0.20|  -0.48|    0.01|   -0.52|  -0.27|   2.89|    13|
|     5|    -0.23|  -0.10|   -0.10|    2.54|  -0.61|  -0.39|    17|

------------------------------------


```r
kable(comuGroup)
```



| comu|descriz                            | group| ampiezza| indVec| indStra| indAgri| indInd|    indTur|
|----:|:----------------------------------|-----:|--------:|------:|-------:|-------:|------:|---------:|
|    1|Ala                                |     1|     8915| 112.10|   13.48|    0.86|  13.76|    346.48|
|    2|Albiano                            |     1|     1528| 116.54|   11.39|    0.00|  46.05|     45.58|
|    7|Avio                               |     1|     4089| 135.00|    8.78|    2.15|  14.88|    166.27|
|   22|Borgo Valsugana                    |     1|     6889| 147.08|   10.20|    0.30|  11.90|    516.73|
|   35|Calliano                           |     1|     1776|  75.23|   11.15|    0.34|  16.08|    434.89|
|   40|Capriana                           |     1|      589| 141.67|    9.34|    0.17|   9.39|   5263.83|
|   46|Castelfondo                        |     1|      634| 132.99|   11.36|    2.52|  12.20|   4964.18|
|   49|Castelnuovo                        |     1|     1047| 108.55|    7.74|    1.15|  45.01|    180.82|
|   51|Cavareno                           |     1|     1047| 128.90|   11.17|    0.38|  13.56|  10762.37|
|   62|Cles                               |     1|     6790| 159.61|   12.58|    0.90|  17.34|    369.09|
|   64|Commezzadura                       |     1|     1002| 132.03|    8.98|    0.40|  12.49|  17529.07|
|   68|Croviana                           |     1|      686| 110.58|   10.64|    0.87|   8.07|   2544.57|
|   79|Dro                                |     1|     4851|  89.56|    9.92|    0.72|   9.85|   1124.03|
|   83|Fiave'                             |     1|     1125| 142.68|   16.98|    0.71|   6.59|   3497.15|
|   88|Fondo                              |     1|     1369| 191.92|   11.54|    1.61|  18.74|   8059.10|
|   89|Fornace                            |     1|     1352|  89.59|   11.32|    0.00|  20.41|    246.35|
|   99|Ivano-Fracena                      |     1|      331|  66.67|   11.18|    0.91|  16.26|    215.00|
|  103|Lavis                              |     1|     8782| 109.83|   10.87|    0.93|  22.00|    389.61|
|  104|Levico Terme                       |     1|     7664| 122.20|   11.59|    0.73|   7.83|  12807.96|
|  108|Lona-Lases                         |     1|      886|  96.03|   21.90|    0.11|  23.20|    498.98|
|  110|Male'                              |     1|     2093| 186.11|   13.86|    0.91|   6.17|   6848.07|
|  111|Malosco                            |     1|      468|  96.51|   19.02|    0.21|  10.56|  16375.05|
|  112|Massimeno                          |     1|      125| 200.00|   11.20|    0.00|   4.30|   4168.00|
|  116|Mezzocorona                        |     1|     5342| 135.45|   11.62|    1.85|  15.21|    333.97|
|  117|Mezzolombardo                      |     1|     7006| 125.25|   13.32|    0.71|  11.28|     78.06|
|  123|Mori                               |     1|     9675| 138.10|    9.00|    0.42|   6.92|    200.27|
|  138|Pelugo                             |     1|      401|  88.41|   10.72|    0.00|   8.15|   3097.46|
|  160|Rovere' della Luna                 |     1|     1590| 134.14|   13.02|    3.14|  16.86|    106.41|
|  167|San Michele all'Adige              |     1|     3039|  91.54|   14.58|    0.89|   3.13|    300.60|
|  170|Sarnonico                          |     1|      770| 125.23|   12.08|    1.04|  10.55|  10098.17|
|  171|Scurelle                           |     1|     1433| 133.04|    8.03|    0.91|  64.97|    438.50|
|  172|Segonzano                          |     1|     1509| 113.77|    9.48|    1.06|   7.40|   2720.12|
|  179|Spiazzo                            |     1|     1227| 143.81|   12.31|    0.24|   9.00|  16567.90|
|  180|Spormaggiore                       |     1|     1280| 118.75|   10.86|    1.25|   9.94|   2313.35|
|  182|Stenico                            |     1|     1168| 130.16|    8.99|    0.77|   8.28|  10749.05|
|  184|Strembo                            |     1|      559| 123.96|   14.85|    0.89|  16.54|  22812.79|
|  199|Tione di Trento                    |     1|     3627| 145.64|   12.68|    0.11|  16.67|    688.43|
|  224|Volano                             |     1|     3179| 117.98|    9.78|    0.98|  16.44|      7.75|
|  225|Zambana                            |     1|     1732| 124.44|   11.95|    1.10|   5.48|     10.95|
|  228|Comano Terme                       |     1|     2971| 108.10|   14.84|    1.18|  15.50|  10352.42|
|  238|Borgo Chiese                       |     1|     2008| 161.31|    5.88|    0.20|  38.77|    682.18|
|  241|Cembra Lisignago                   |     1|     2317| 127.93|   10.88|    1.34|  10.39|   1157.94|
|  243|Madruzzo                           |     1|     2913| 113.24|   10.40|    1.99|   7.34|   1000.28|
|  244|Porte di Rendena                   |     1|     1775| 118.87|   13.58|    0.39|  10.95|   4260.89|
|    3|Aldeno                             |     2|     3084| 119.54|    5.80|    2.59|   7.11|      8.09|
|    9|Baselga di Pine'                   |     2|     4983| 134.97|    6.66|    0.48|   7.31|   8971.03|
|   11|Bedollo                            |     2|     1483| 173.06|    2.49|    0.27|   4.79|   5973.59|
|   13|Besenello                          |     2|     2635|  85.31|    4.10|    1.14|   9.64|     38.48|
|   15|Bieno                              |     2|      419| 211.32|    7.40|    0.72|   7.95|   6510.90|
|   17|Bleggio Superiore                  |     2|     1492| 187.68|    7.51|    1.27|   8.08|   5414.60|
|   18|Bocenago                           |     2|      402| 210.53|    6.97|    0.00|   7.31|  14364.99|
|   21|Bondone                            |     2|      679| 176.60|    2.21|    0.29|   6.17|   2394.07|
|   25|Brentonico                         |     2|     3957| 140.13|    7.46|    0.99|   4.32|   5916.75|
|   26|Bresimo                            |     2|      255| 200.00|    0.78|    1.57|   2.35|   1408.73|
|   32|Calceranica al Lago                |     2|     1379| 125.47|    5.37|    0.44|  16.27|  21728.13|
|   34|Caldonazzo                         |     2|     3552| 103.15|    4.19|    0.93|   7.22|   5806.92|
|   38|Canal San Bovo                     |     2|     1522| 204.74|    2.50|    0.79|   9.19|   4580.01|
|   43|Carzano                            |     2|      521| 136.49|    3.84|    2.11|   7.35|    142.36|
|   45|Castel Condino                     |     2|      239| 127.66|    2.93|    0.42|   7.75|   3733.47|
|   47|Castello-Molina di Fiemme          |     2|     2288| 141.79|    5.07|    0.61|   9.24|   8880.55|
|   48|Castello Tesino                    |     2|     1227| 367.65|    1.55|    0.49|   4.16|   8375.82|
|   50|Cavalese                           |     2|     4053| 133.98|    9.57|    0.37|   9.22|  16746.43|
|   52|Cavedago                           |     2|      517| 159.21|    4.06|    0.77|   6.97|   7797.70|
|   53|Cavedine                           |     2|     2948| 144.64|    7.16|    1.22|   7.49|    927.05|
|   54|Cavizzana                          |     2|      257|  90.48|    3.50|    1.56|   1.25|   2504.25|
|   58|Cimone                             |     2|      727|  92.19|    4.95|    0.96|   2.97|    431.40|
|   59|Cinte Tesino                       |     2|      372| 369.70|    1.34|    0.54|  26.52|   5105.66|
|   60|Cis                                |     2|      310| 144.90|    3.55|    2.26|  11.89|   1215.16|
|   61|Civezzano                          |     2|     4010|  97.72|    3.47|    0.40|   4.73|    450.25|
|   70|Daiano                             |     2|      644| 158.43|    2.33|    0.31|   3.85|  11636.47|
|   78|Drena                              |     2|      550| 150.79|    5.27|    0.73|   5.99|   1566.24|
|   81|Fai della Paganella                |     2|      904| 222.92|    3.65|    0.88|   5.80|  25207.65|
|   85|Fierozzo-Vlar÷tz                   |     2|      477| 116.28|    0.00|    5.03|   3.73|   2426.50|
|   90|Frassilongo-Garait                 |     2|      333| 186.84|    0.60|    4.50|   7.25|   8593.19|
|   91|Garniga Terme                      |     2|      374| 176.00|    3.21|    0.00|   1.23|   3388.60|
|   92|Giovo                              |     2|     2510| 113.11|    1.59|    2.87|   7.15|    686.68|
|   95|Grigno                             |     2|     2221| 217.11|    5.94|    0.27|  24.52|    219.85|
|   97|Imer                               |     2|     1161| 161.15|    3.27|    0.26|  19.23|   2269.94|
|   98|Isera                              |     2|     2629| 137.35|    4.64|    0.65|   7.73|    192.00|
|  109|Luserna-LusÚrn                     |     2|      274| 266.67|    1.09|    0.00|   1.82|   5683.81|
|  115|Mezzano                            |     2|     1633| 155.51|    2.39|    0.55|   9.33|   2702.02|
|  118|Moena-Moena                        |     2|     2677| 185.15|    5.98|    0.34|   7.70|  25995.01|
|  127|Nogaredo                           |     2|     2014| 125.16|    4.27|    0.99|   6.28|    216.86|
|  128|Nomi                               |     2|     1282| 171.29|    5.93|    1.56|   4.73|      0.00|
|  129|Novaledo                           |     2|     1069|  94.05|    4.02|    0.75|  22.81|    346.59|
|  130|Ospedaletto                        |     2|      825| 147.93|    7.39|    1.21|   8.98|     16.95|
|  131|Ossana                             |     2|      863| 145.38|    7.07|    0.35|  20.51|  17492.87|
|  133|Pal¨ del Fersina-Palai en Bersntol |     2|      178| 480.00|    1.69|    2.25|   2.78|   4943.18|
|  134|Panchia'                           |     2|      813| 115.00|    5.29|    0.12|  11.55|  11404.37|
|  135|Ronzo-Chienis                      |     2|      981| 160.15|    4.59|    1.53|   8.35|   7164.94|
|  136|Peio                               |     2|     1862| 163.67|    3.87|    0.75|   8.79|  23673.79|
|  137|Pellizzano                         |     2|      730| 246.51|    5.62|    0.27|   3.21|  15800.54|
|  142|Pieve Tesino                       |     2|      663| 298.61|    2.87|    0.60|   7.85|  11438.08|
|  144|Pomarolo                           |     2|     2478| 102.60|    6.78|    0.65|   1.55|    179.06|
|  147|Predazzo                           |     2|     4502| 156.68|    7.09|    0.24|  12.16|  14244.39|
|  150|Rabbi                              |     2|     1384| 180.84|    2.60|    2.46|   8.30|   7295.76|
|  154|Romallo                            |     2|      621| 197.47|    9.02|    3.38|   4.42|    801.81|
|  155|Romeno                             |     2|     1381| 157.95|    9.20|    1.96|   8.78|   6510.76|
|  156|Roncegno Terme                     |     2|     2860| 126.35|    4.65|    1.22|   7.39|   3680.18|
|  157|Ronchi Valsugana                   |     2|      435| 145.76|    1.84|    2.07|   3.05|    346.74|
|  162|Ruffre'-Mendola                    |     2|      419| 203.92|    4.53|    0.48|   4.57|  17995.19|
|  163|Rumo                               |     2|      814| 160.18|   10.32|    2.09|   9.95|   5633.09|
|  164|Sagron Mis                         |     2|      189| 288.24|    2.65|    0.00|   1.59|   6228.04|
|  165|Samone                             |     2|      554| 138.37|    7.04|    1.99|   3.87|     61.33|
|  168|Sant'Orsola Terme                  |     2|     1107| 104.62|    5.24|    1.81|   3.46|   1013.81|
|  173|Sfruz                              |     2|      335| 158.49|    3.88|    1.19|   3.02|  15452.10|
|  177|Sover                              |     2|      837| 188.79|    4.54|    0.12|   5.78|   2630.40|
|  183|Storo                              |     2|     4680| 117.91|    6.41|    0.38|  16.73|    318.82|
|  188|Telve                              |     2|     1906| 169.92|    5.77|    1.10|   8.60|   1058.64|
|  189|Telve di Sopra                     |     2|      591| 184.00|    3.72|    0.68|   5.68|      7.53|
|  190|Tenna                              |     2|      988| 169.17|    3.34|    0.71|   4.36|   5405.42|
|  191|Tenno                              |     2|     2038| 114.98|    6.58|    0.88|   2.06|   3697.98|
|  193|Terragnolo                         |     2|      731| 206.82|    3.42|    0.14|   2.97|    471.06|
|  195|Terzolas                           |     2|      622| 157.14|    6.59|    0.96|  10.56|   4485.04|
|  196|Tesero                             |     2|     2925| 122.66|    6.56|    0.38|  19.84|  12808.81|
|  202|Torcegno                           |     2|      698| 159.55|    1.86|    1.00|   8.09|    932.52|
|  203|Trambileno                         |     2|     1413| 112.88|    3.89|    0.35|  14.03|     82.07|
|  209|Valfloriana                        |     2|      519| 233.33|    5.20|    0.19|   1.93|   4033.46|
|  210|Vallarsa                           |     2|     1328| 214.56|    2.94|    0.90|   3.43|   1625.94|
|  211|Varena                             |     2|      864| 143.75|    6.48|    0.46|   9.28|  15948.42|
|  216|Vignola-Falesina                   |     2|      165| 104.17|    3.64|    1.82|   4.87|  12415.95|
|  222|Villa Lagarina                     |     2|     3766| 104.73|    5.50|    0.21|  10.34|    419.23|
|  226|Ziano di Fiemme                    |     2|     1689| 123.85|    4.20|    0.18|  19.74|  15834.28|
|  229|Ledro                              |     2|     5350| 143.15|    7.25|    0.36|  10.85|  15939.85|
|  231|San Lorenzo Dorsino                |     2|     1583| 158.77|    4.74|    0.32|   9.63|   8629.72|
|  232|Valdaone                           |     2|     1196| 193.20|    0.92|    0.67|   8.76|   3762.76|
|  234|Pieve di Bono-Prezzo               |     2|     1440| 196.63|    3.47|    0.28|  12.14|    606.50|
|  235|Altavalle                          |     2|     1633| 159.09|    7.04|    1.71|  11.01|   2922.61|
|  236|Altopiano della Vigolana           |     2|     4936| 102.19|    3.28|    0.93|   6.10|   1520.34|
|  237|Amblar-Don                         |     2|      512| 159.15|    7.23|    0.78|  17.14|   9656.04|
|  239|Borgo Lares                        |     2|      680| 186.90|    3.24|    0.59|  22.17|   2188.62|
|  240|Castel Ivano                       |     2|     2954| 146.65|    7.58|    1.25|   5.51|    468.58|
|  245|Primiero San Martino di Castrozza  |     2|     5372| 146.94|    5.45|    0.60|   7.72|  19415.95|
|  246|Sella Giudicarie                   |     2|     2951| 136.93|    2.58|    0.47|   9.01|   5928.45|
|  247|Tre Ville                          |     2|     1460| 168.39|    3.63|    0.27|   7.95|  18914.13|
|  248|Vallelaghi                         |     2|     5034| 119.68|    5.62|    0.89|   4.95|    874.08|
|    5|Andalo                             |     3|     1061| 116.46|    5.47|    0.28|   9.30|  91071.02|
|   29|Caderzone Terme                    |     3|      665| 142.72|    9.62|    1.20|   6.51|  28165.15|
|   36|Campitello di Fassa-Ciampedel      |     3|      725| 155.32|    4.97|    0.69|   3.25|  73676.75|
|   39|Canazei-Cianacei                   |     3|     1915| 107.57|    5.69|    0.05|   6.37|  56105.33|
|   41|Carano                             |     3|     1087| 102.62|    5.15|    0.46|  10.16|  34047.30|
|   42|Carisolo                           |     3|      972| 129.53|    7.92|    0.21|  13.82|  41421.68|
|   87|Folgaria                           |     3|     3144| 221.91|    5.98|    0.38|   5.11|  42530.96|
|   93|Giustino                           |     3|      731| 153.61|    9.71|    0.55|   7.66|  21304.64|
|  102|Lavarone                           |     3|     1120| 169.54|    3.48|    0.36|   7.39|  47280.72|
|  113|Mazzin-Mazin                       |     3|      555| 112.66|    6.49|    0.36|   9.96|  68210.78|
|  114|Mezzana                            |     3|      893| 146.15|    6.38|    0.67|   7.14| 107238.36|
|  120|Molveno                            |     3|     1138| 166.00|    4.22|    0.00|   5.55|  37738.80|
|  124|Nago-Torbole                       |     3|     2857| 130.54|   12.88|    0.39|   3.49|  26226.80|
|  143|Pinzolo                            |     3|     3091| 161.98|    6.41|    0.29|   8.16|  57426.08|
|  145|Pozza di Fassa-Poza                |     3|     2252|  82.20|    5.95|    0.62|   9.98|  33655.28|
|  159|Ronzone                            |     3|      419| 191.38|   11.22|    0.24|   4.02|  34422.54|
|  176|Soraga-Soraga                      |     3|      723|  89.38|    8.02|    0.83|  12.51|  29477.75|
|  213|Vermiglio                          |     3|     1911| 123.84|    6.70|    0.37|   5.92|  29462.73|
|  217|Vigo di Fassa-Vich                 |     3|     1225| 100.99|    7.84|    0.73|   6.34|  36224.39|
|  233|Dimaro Folgarida                   |     3|     2206| 126.95|   13.33|    0.54|  11.56|  45519.18|
|    6|Arco                               |     4|    17104| 132.87|    9.80|    0.36|  17.61|   3910.11|
|  139|Pergine Valsugana                  |     4|    20823| 108.77|    9.21|    0.37|   9.28|    962.11|
|  153|Riva del Garda                     |     4|    16757| 139.83|   12.60|    0.19|   8.75|   9388.52|
|  161|Rovereto                           |     4|    38464| 149.03|   12.96|    0.08|  15.01|    392.55|
|   27|Brez                               |     5|      731| 160.53|    5.61|    6.70|   3.63|   3275.17|
|   30|Cagno'                             |     5|      339| 190.00|    8.26|    5.60|   2.06|    719.54|
|   33|Caldes                             |     5|     1105| 131.49|    8.51|    3.62|   7.47|   3158.62|
|   37|Campodenno                         |     5|     1488| 137.14|    5.31|    6.32|   4.14|    305.81|
|   63|Cloz                               |     5|      724| 167.68|   18.37|    5.39|  11.50|   1302.90|
|   71|Dambel                             |     5|      433| 134.33|    8.31|    9.24|   1.19|    865.35|
|   74|Denno                              |     5|     1292| 125.69|   14.24|    4.18|   6.15|    403.11|
|   80|Faedo                              |     5|      617| 115.05|    6.97|    4.38|   3.24|   2235.74|
|  106|Livo                               |     5|      868| 108.22|    7.49|    4.61|   5.33|    359.70|
|  126|Nave San Rocco                     |     5|     1405|  91.49|    9.04|    4.06|   1.98|     41.36|
|  152|Revo'                              |     5|     1241| 167.79|    9.02|    5.00|   7.11|    300.97|
|  169|Sanzeno                            |     5|      931| 162.90|    7.84|    7.73|   1.51|   1609.95|
|  181|Sporminore                         |     5|      713| 161.00|    7.01|    6.31|   1.08|    394.76|
|  200|Ton                                |     5|     1346| 115.02|    6.84|    3.64|   8.66|    269.74|
|  230|Predaia                            |     5|     6567| 120.36|    9.94|    3.58|  11.49|   4578.99|
|  242|Contà                              |     5|     1438| 189.58|    7.79|    5.63|   2.44|   2160.08|
|  249|Ville d'Anaunia                    |     5|     4936| 162.37|    7.84|    5.31|  10.52|    402.60|

----------------------------------


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

