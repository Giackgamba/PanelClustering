# Panel Clusters

## Obiettivi
Obiettivo di questa analisi è suddividere i comuni trentini in un congruo numero di cluster, secondo alcuni indicatori individuati.

## Metodologia
La clusterizzazione è effettuata con il metodo del *clustering gerarchico*, cioè calcolando la distanza tra ciascun comune e aggregando via via i comuni/cluster più vicini fino ad arrivare alla numerosità individuata.  
Il metodo di calcolo della distanza utilizzato è quello del *massimo*, mentre il metodo di aggregazione è il *metodo di Ward*

## Dati
I dati sono stati recuperati dal DB di ISPAT, in fecendo riferimento all'ultimo anno disponibile (solitamente il 2014),  ma utilizzando i confini al 2016.

## Risultati
I risultati sono visibili nel csv risultante ('clustersNuovi.csv')