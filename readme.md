Results of the 2017 French presidential election (commune level)
================
Francesco Bailo
May 12, 2017

Le Pen vs Macron (first and second round)
=========================================

![plots](https://github.com/fraba/french_pres_election_17/raw/master/figures/fig2.jpg)

First round
===========

![plots](https://github.com/fraba/french_pres_election_17/raw/master/figures/fig1.jpg)

Data collection
===============

Result pages were downloaded from the [website](http://elections.interieur.gouv.fr/presidentielle-2017/) of the French Ministry of Interior with `wget -r -w 5 --accept html http://elections.interieur.gouv.fr/presidentielle-2017/` and then parsed with the following R function

``` r
parse <- function(this_file, this_uri = NULL) {
  
  if (is.null(this_uri)) {
    system(paste0("scp francesco@XXX.XXX.XX.XXX:", this_file, " tmp.html"))
    this_page <- read_html("tmp.html")  
  } else {
    this_file <- this_uri
    this_page <- try({read_html(this_uri)})
    if (any(class(this_page) %in% 'try-error')) return(NULL)
  }
  
  code_region <- str_extract_all(this_file, "\\b\\d{3}\\b")[[1]][1]
  code_department <- str_extract_all(this_file, "\\b\\d{3}\\b")[[1]][2]
  code_commune <- str_extract(this_file, "\\d{6}")
  uri <- gsub("/mnt/francesco/fr_election17/", "", this_file)
  
  print(code_commune)
  
  desc_commune <- 
    this_page %>%
    html_node(xpath = '//*[@id="top"]/div[2]/div[1]/div[4]/div/h3[1]') %>%
    html_text() %>%
    str_replace("Commune de ", "") %>%
    str_trim()
  
  df_part1 <- 
    data.frame(uri, code_region, code_department, code_commune, desc_commune, stringsAsFactors = FALSE)
  
  tables <- 
    this_page %>% 
    html_nodes("table") %>%
    html_table(header = TRUE)
  
  df_part2 <- as.data.frame(t(tables[[2]]['Voix']))
  this_columns <- str_trim(tables[[2]][['Liste des candidats']])
  last_lower_chr <- sapply(this_columns, FUN = function(x) max(which(grepl("[a-z]", strsplit(x, split = "")[[1]]))))
  last_chr <- sapply(this_columns, nchar)
  this_columns <- mapply(substr, this_columns, last_lower_chr + 2, last_chr)
  colnames(df_part2) <- this_columns
  
  df_part3 <- as.data.frame(t(tables[[3]]['Nombre']))
  colnames(df_part3) <- tables[[3]][[1]]
  
  this_results <- cbind(df_part1, df_part2, df_part3)
  row.names(this_results) <- NULL 
  unlink("tmp.html")
  return(this_results)
}
```

and this code (replace `results_fr_20170423.RData` with `results_fr_20170507.RData` for results of the second round)

``` r
output_file <- 'results_fr_20170423.RData'

all_files <- read.table(pipe('ssh francesco@XXX.XXX.XX.XXX "find /mnt/francesco/fr_election17/elections.interieur.gouv.fr -name *.html"'))
all_files <- as.character(all_files$V1)
all_files <- all_files[grepl("\\d{6}", all_files)]
all_remote_codes <- sapply(all_files, FUN = function(x) str_extract_all(x, "\\d{6}")[[1]], USE.NAMES = F) 
all_files <- 
  all_files[!all_remote_codes %in% results_fr_20170423$code_commune]

for (this_file in all_files) {
  results_fr_20170423 <- rbind(results_fr_20170423, parse(this_file))
  save(results_fr_20170423, file = output_file)
}
```

Data preparation
================

Addition of arrondissements' INSEE codes for Paris, Lyon and Marseille
----------------------------------------------------------------------

``` r
which_arr <- which(is.na(results_fr_20170423$desc_commune))
uri_arr <- results_fr_20170423$uri[which_arr]
insee_arr <- list('Paris' = list(code = '075056', range = 75101:75120), 
                  "Lyon" = list(code = '069123', range = 69381:69389), 
                  "Marseille" = list(code = '013055', range = 13201:13216))
for (i in 1:length(insee_arr)) {
  results_fr_20170423$desc_commune[results_fr_20170423$code_commune == insee_arr[[i]]$code] <- 
    names(insee_arr)[i]
  ordered_uri <- results_fr_20170423$uri[results_fr_20170423$code_commune == insee_arr[[i]]$code]
  ordered_uri <- ordered_uri[order(ordered_uri)]
  for (j in 1:length(ordered_uri)) {
    results_fr_20170423$code_commune[results_fr_20170423$uri == ordered_uri[j]] <- 
      sprintf("%06d", insee_arr[[i]]$range[j])
  }
}

results_fr_20170423 <- results_fr_20170423[!results_fr_20170423$code_commune %in% c('0000NA', '069846'),] 
```

Addition of Corse's INSEE codes
-------------------------------

``` r
## Corse
uri_commune_corse <- results_fr_20170423$uri[grepl("/02B/|/02A/", results_fr_20170423$uri)]

require(stringr)
for(uri in uri_commune_corse){
  results_fr_20170423$code_department[results_fr_20170423$uri == uri] <- 
    str_extract(results_fr_20170423$uri[results_fr_20170423$uri == uri], "02[A-B]")
  results_fr_20170423$code_commune[results_fr_20170423$uri == uri] <- 
    str_extract(results_fr_20170423$uri[results_fr_20170423$uri == uri], "02[A-B]\\d{3}")
}
```

Transformation of INSEE code for a few overseas territories (it differed from codes in [shapefile](http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20170111-shp.zip))
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
results_fr_20170423$code_commune[grepl("^97", results_fr_20170423$code_commune)] <- 
  gsub("97[0-9]", "97", results_fr_20170423$code_commune[grepl("^97", results_fr_20170423$code_commune)])

results_fr_20170423$code_commune[grepl("^976", results_fr_20170423$code_department)] <- 
  gsub("975", "976", results_fr_20170423$code_commune[grepl("^976", results_fr_20170423$code_department)])
```

Elimination of duplicate rows and creation of join column compatible with [this shapefile](http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20170111-shp.zip) and [this shapefile](http://osm13.openstreetmap.fr/~cquest/openfla/export/arrondissements-municipaux-20160128-shp.zip) (`Arrondissements Municipaux (janvier 2016)`).
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
results_fr_20170423 <- results_fr_20170423[!duplicated(results_fr_20170423$code_commune),]
results_fr_20170423$code_commune_join <- gsub("^0", "", results_fr_20170423$code_commune)
```

Conversion to numeric and addition to percentage column
-------------------------------------------------------

``` r
results_fr_20170423[,6:22] <- apply(results_fr_20170423[,6:22], 2,
                                    FUN = function(x) as.numeric(gsub("[^0-9]", "", x)))

for (n in colnames(results_fr_20170423)[6:16]) {
  results_fr_20170423[[paste0(n, " %")]] <- results_fr_20170423[[n]] / results_fr_20170423[['Exprimés']]
}
```

Results
-------

### First round

The resulting file (`results_fr_20170423`) contains 35,537 rows (communes and arrondissements) and looks like this

| uri                                                                 | code\_region | code\_department | code\_commune | desc\_commune     |  MÉLENCHON|  LE PEN|  MACRON|  FILLON|  HAMON|  DUPONT-AIGNAN|  LASSALLE|  POUTOU|  ARTHAUD|  CHEMINADE|  ASSELINEAU|  Inscrits|  Abstentions|  Votants|  Blancs|  Nuls|  Exprimés| code\_commune\_join |  MÉLENCHON %|   LE PEN %|   MACRON %|   FILLON %|    HAMON %|  DUPONT-AIGNAN %|  LASSALLE %|   POUTOU %|  ARTHAUD %|  CHEMINADE %|  ASSELINEAU %|
|:--------------------------------------------------------------------|:-------------|:-----------------|:--------------|:------------------|----------:|-------:|-------:|-------:|------:|--------------:|---------:|-------:|--------:|----------:|-----------:|---------:|------------:|--------:|-------:|-----:|---------:|:--------------------|------------:|----------:|----------:|----------:|----------:|----------------:|-----------:|----------:|----------:|------------:|-------------:|
| elections.interieur.gouv.fr/presidentielle-2017/000/975/975501.html | 000          | 975              | 97501         | Miquelon-Langlade |         89|      60|      42|      27|     18|              9|         6|       4|        2|          2|           2|       495|          226|      269|       5|     3|       261| 97501               |    0.3409962|  0.2298851|  0.1609195|  0.1034483|  0.0689655|        0.0344828|   0.0229885|  0.0153257|  0.0076628|    0.0076628|     0.0076628|
| elections.interieur.gouv.fr/presidentielle-2017/000/975/975502.html | 000          | 975              | 97502         | Saint-Pierre      |        844|     418|     431|     234|    199|             70|        48|      60|       26|          7|          34|      4471|         2012|     2459|      65|    23|      2371| 97502               |    0.3559679|  0.1762969|  0.1817798|  0.0986925|  0.0839308|        0.0295234|   0.0202446|  0.0253058|  0.0109658|    0.0029523|     0.0143399|
| elections.interieur.gouv.fr/presidentielle-2017/000/977/977801.html | 000          | 977              | 97801         | Saint-Martin      |        854|    1203|    1117|    1376|    176|            121|        41|      66|       26|         15|          65|     20153|        14844|     5309|     163|    86|      5060| 97801               |    0.1687747|  0.2377470|  0.2207510|  0.2719368|  0.0347826|        0.0239130|   0.0081028|  0.0130435|  0.0051383|    0.0029644|     0.0128458|
| elections.interieur.gouv.fr/presidentielle-2017/000/977/977701.html | 000          | 977              | 97701         | Saint-Barthélémy  |        299|     631|     455|    1142|     71|             95|        27|      26|        9|          3|          47|      5232|         2358|     2874|      48|    21|      2805| 97701               |    0.1065954|  0.2249554|  0.1622103|  0.4071301|  0.0253119|        0.0338681|   0.0096257|  0.0092692|  0.0032086|    0.0010695|     0.0167558|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987029.html | 000          | 987              | 987029        | Moorea-Maiao      |        387|    1888|     526|    1411|    149|             83|        29|      49|       21|          8|          48|     13121|         8271|     4850|     125|   126|      4599| 987029              |    0.0841487|  0.4105240|  0.1143727|  0.3068058|  0.0323983|        0.0180474|   0.0063057|  0.0106545|  0.0045662|    0.0017395|     0.0104371|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987057.html | 000          | 987              | 987057        | Ua-Pou            |         48|     244|      68|     276|     12|             18|         2|      12|       17|          3|          15|      1560|          826|      734|       6|    13|       715| 987057              |    0.0671329|  0.3412587|  0.0951049|  0.3860140|  0.0167832|        0.0251748|   0.0027972|  0.0167832|  0.0237762|    0.0041958|     0.0209790|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987054.html | 000          | 987              | 987054        | Tumaraa           |         57|     448|      99|     782|     32|             26|         4|       8|        5|          3|          12|      3107|         1589|     1518|      20|    22|      1476| 987054              |    0.0386179|  0.3035230|  0.0670732|  0.5298103|  0.0216802|        0.0176152|   0.0027100|  0.0054201|  0.0033875|    0.0020325|     0.0081301|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987030.html | 000          | 987              | 987030        | Napuka            |          3|      64|       2|      20|      0|              6|         0|       0|        0|          1|           0|       272|          161|      111|       4|    11|        96| 987030              |    0.0312500|  0.6666667|  0.0208333|  0.2083333|  0.0000000|        0.0625000|   0.0000000|  0.0000000|  0.0000000|    0.0104167|     0.0000000|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987032.html | 000          | 987              | 987032        | Nukutavake        |          5|      69|      11|      38|      5|              0|         0|       5|        1|          0|           1|       291|          152|      139|       2|     2|       135| 987032              |    0.0370370|  0.5111111|  0.0814815|  0.2814815|  0.0370370|        0.0000000|   0.0000000|  0.0370370|  0.0074074|    0.0000000|     0.0074074|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987046.html | 000          | 987              | 987046        | Tahuata           |          6|     151|      23|      76|      1|              2|         1|       1|        3|          0|           1|       602|          331|      271|       0|     6|       265| 987046              |    0.0226415|  0.5698113|  0.0867925|  0.2867925|  0.0037736|        0.0075472|   0.0037736|  0.0037736|  0.0113208|    0.0000000|     0.0037736|

In total, 36,071,776 votes are assigned to a candidate.

### Second round

The resulting file (`results_fr_20170507`) contains 35,539 rows (communes and arrondissements) and looks like this

| uri                                                                 | code\_region | code\_department | code\_commune | desc\_commune     |  MACRON|  LE PEN|  Inscrits|  Abstentions|  Votants|  Blancs|  Nuls|  Exprimés| code\_commune\_join |   MACRON %|   LE PEN %|
|:--------------------------------------------------------------------|:-------------|:-----------------|:--------------|:------------------|-------:|-------:|---------:|------------:|--------:|-------:|-----:|---------:|:--------------------|----------:|----------:|
| elections.interieur.gouv.fr/presidentielle-2017/000/975/975501.html | 000          | 975              | 97501         | Miquelon-Langlade |     146|     104|       495|          205|      290|      30|    10|       250| 97501               |  0.5840000|  0.4160000|
| elections.interieur.gouv.fr/presidentielle-2017/000/975/975502.html | 000          | 975              | 97502         | Saint-Pierre      |    1321|     747|      4468|         1979|     2489|     276|   145|      2068| 97502               |  0.6387814|  0.3612186|
| elections.interieur.gouv.fr/presidentielle-2017/000/977/977801.html | 000          | 977              | 97801         | Saint-Martin      |    3857|    1826|     20152|        13919|     6233|     383|   167|      5683| 97801               |  0.6786908|  0.3213092|
| elections.interieur.gouv.fr/presidentielle-2017/000/977/977701.html | 000          | 977              | 97701         | Saint-Barthélémy  |    1425|    1014|      5295|         2535|     2760|     255|    66|      2439| 97701               |  0.5842558|  0.4157442|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987029.html | 000          | 987              | 987029        | Moorea-Maiao      |    2850|    2950|     13118|         6937|     6181|     220|   161|      5800| 987029              |  0.4913793|  0.5086207|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987057.html | 000          | 987              | 987057        | Ua-Pou            |     416|     464|      1565|          660|      905|      11|    14|       880| 987057              |  0.4727273|  0.5272727|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987054.html | 000          | 987              | 987054        | Tumaraa           |    1168|     625|      3113|         1127|     1986|      17|   176|      1793| 987054              |  0.6514222|  0.3485778|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987030.html | 000          | 987              | 987030        | Napuka            |      24|     113|       272|          131|      141|       0|     4|       137| 987030              |  0.1751825|  0.8248175|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987032.html | 000          | 987              | 987032        | Nukutavake        |      76|      78|       291|          128|      163|       9|     0|       154| 987032              |  0.4935065|  0.5064935|
| elections.interieur.gouv.fr/presidentielle-2017/000/987/987046.html | 000          | 987              | 987046        | Tahuata           |     149|     167|       602|          281|      321|       5|     0|       316| 987046              |  0.4715190|  0.5284810|

In total, 31,468,034 votes are assigned to a candidate.

Census data
===========

The file `results_fr_20170423_wt_demog` also contains 2013 census data based on the 2017 administrative division at the commune level. Two census datafiles were used: `BTT_TD_IMG3A_2013.txt` and `BTT_TD_IMG2A_2013.txt`. They are available [here](https://www.insee.fr/fr/information/2409289).

The number of immigrants, retired and unemployed from each commune was computed with

``` r
require(dplyr)
pop_immi_empl_by_commune <- 
  BTT_TD_IMG3A_2013 %>%
  group_by(NIVGEO, CODGEO) %>%
  summarize(
    immigrants = sum(NB[IMMI == 1]),
    nonimmigrants = sum(NB[IMMI == 2])
  )

unemployment_by_commune <- 
  BTT_TD_IMG2A_2013 %>%
  group_by(NIVGEO, CODGEO) %>%
  summarize(
    actifs = sum(NB[TACTR == 11]),
    chomeurs = sum(NB[TACTR == 12]),
    retraites_IMG2A = sum(NB[TACTR == 21])
    )
```

Because the the number of communes and their boundaries are changed between 2013 and 2017, I randomly distributed `n` points within the boundaries of the 2013 communes (with `n` corresponding to the number of people in the different categories) and counted the number of random points within each boundary of the 2017 communes.

The resulting file contains 35,454 rows (communes and arrondissements) and looks like this

| code\_commune\_join | uri                                                                 | code\_region | code\_department | code\_commune | desc\_commune           |  MÉLENCHON|  LE PEN|  MACRON|  FILLON|  HAMON|  DUPONT-AIGNAN|  LASSALLE|  POUTOU|  ARTHAUD|  CHEMINADE|  ASSELINEAU|  Inscrits|  Abstentions|  Votants|  Blancs|  Nuls|  Exprimés|  MÉLENCHON %|   LE PEN %|   MACRON %|   FILLON %|    HAMON %|  DUPONT-AIGNAN %|  LASSALLE %|   POUTOU %|  ARTHAUD %|  CHEMINADE %|  ASSELINEAU %|  immigrants|  retraites\_img2a|  chomeurs|  actifs|  nb\_index|  immigrants\_50km|  retraites\_img2a\_50km|  chomeurs\_50km|  actifs\_50km|  inscrits\_50km|  surf\_ha|
|:--------------------|:--------------------------------------------------------------------|:-------------|:-----------------|:--------------|:------------------------|----------:|-------:|-------:|-------:|------:|--------------:|---------:|-------:|--------:|----------:|-----------:|---------:|------------:|--------:|-------:|-----:|---------:|------------:|----------:|----------:|----------:|----------:|----------------:|-----------:|----------:|----------:|------------:|-------------:|-----------:|-----------------:|---------:|-------:|----------:|-----------------:|-----------------------:|---------------:|-------------:|---------------:|---------:|
| 01001               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001001.html | 084          | 001              | 001001        | L'Abergement-Clémenciat |         59|     126|     119|     110|     29|             34|         2|       4|        4|          2|           6|       598|           92|      506|       2|     9|       495|    0.1191919|  0.2545455|  0.2404040|  0.2222222|  0.0585859|        0.0686869|   0.0040404|  0.0080808|  0.0080808|    0.0040404|     0.0121212|          31|               182|        30|     323|      11652|            267998|                  411576|          127923|        879423|         1563234|      1567|
| 01002               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001002.html | 084          | 001              | 001002        | L'Abergement-de-Varey   |         33|      48|      37|      34|     13|              6|         0|       2|        2|          0|           1|       209|           25|      184|       6|     2|       176|    0.1875000|  0.2727273|  0.2102273|  0.1931818|  0.0738636|        0.0340909|   0.0000000|  0.0113636|  0.0113636|    0.0000000|     0.0056818|          12|                57|         9|     109|      25174|            470615|                  607856|          196217|       1408144|          801504|       909|
| 01004               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001004.html | 084          | 001              | 001004        | Ambérieu-en-Bugey       |       1412|    1667|    1332|    1084|    344|            346|        60|      91|       40|          5|          71|      8586|         1962|     6624|     114|    58|      6452|    0.2188469|  0.2583695|  0.2064476|  0.1680099|  0.0533168|        0.0536268|   0.0092994|  0.0141042|  0.0061996|    0.0007750|     0.0110043|        1418|              2905|       958|    5872|      25190|            421234|                  582357|          182263|       1304080|         1577516|      2449|
| 01005               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001005.html | 084          | 001              | 001005        | Ambérieux-en-Dombes     |        126|     306|     191|     197|     37|             45|         6|      10|        5|          0|          10|      1172|          215|      957|      21|     3|       933|    0.1350482|  0.3279743|  0.2047160|  0.2111468|  0.0396570|        0.0482315|   0.0064309|  0.0107181|  0.0053591|    0.0000000|     0.0107181|          61|               320|        67|     781|      25239|            248659|                  386194|          119816|        826488|         1854925|      1602|
| 01006               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001006.html | 084          | 001              | 001006        | Ambléon                 |         19|      18|      15|      14|      3|              4|         1|       2|        1|          0|           0|        99|           20|       79|       2|     0|        77|    0.2467532|  0.2337662|  0.1948052|  0.1818182|  0.0389610|        0.0519481|   0.0129870|  0.0259740|  0.0129870|    0.0000000|     0.0000000|          23|                30|         2|      52|      29586|            225939|                  458840|          124414|        869552|          911070|       597|
| 01007               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001007.html | 084          | 001              | 001007        | Ambronay                |        296|     458|     348|     233|     82|             80|        13|      17|        9|          1|          11|      1880|          268|     1612|      53|    11|      1548|    0.1912145|  0.2958656|  0.2248062|  0.1505168|  0.0529716|        0.0516796|   0.0083979|  0.0109819|  0.0058140|    0.0006460|     0.0071059|          97|               488|       105|    1138|      25193|            465756|                  589640|          192519|       1390830|         1481341|      3364|
| 01008               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001008.html | 084          | 001              | 001008        | Ambutrix                |         89|     135|      95|      84|     23|             28|         3|       8|        3|          3|           2|       581|           91|      490|      12|     5|       473|    0.1881607|  0.2854123|  0.2008457|  0.1775899|  0.0486258|        0.0591966|   0.0063425|  0.0169133|  0.0063425|    0.0063425|     0.0042283|          13|               121|        32|     382|      25182|            344925|                  482680|          148416|       1094388|         1721686|       522|
| 01009               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001009.html | 084          | 001              | 001009        | Andert-et-Condon        |         39|      40|      55|      44|      8|              6|         8|       3|        0|          0|           6|       254|           41|      213|       1|     3|       209|    0.1866029|  0.1913876|  0.2631579|  0.2105263|  0.0382775|        0.0287081|   0.0382775|  0.0143541|  0.0000000|    0.0000000|     0.0287081|          12|                90|        12|     144|      29611|            210457|                  450467|          119734|        838619|          829583|       701|
| 01010               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001010.html | 084          | 001              | 001010        | Anglefort               |        103|     207|     110|      83|     20|             33|        14|       5|        3|          1|          10|       738|          136|      602|       2|    11|       589|    0.1748727|  0.3514431|  0.1867572|  0.1409168|  0.0339559|        0.0560272|   0.0237691|  0.0084890|  0.0050934|    0.0016978|     0.0169779|          70|               231|        62|     450|      32973|            306535|                  544607|          156572|       1079261|          770913|      2963|
| 01011               | elections.interieur.gouv.fr/presidentielle-2017/084/001/001011.html | 084          | 001              | 001011        | Apremont                |         30|      64|      49|      46|     13|              9|         4|       4|        4|          0|           1|       275|           43|      232|       6|     2|       224|    0.1339286|  0.2857143|  0.2187500|  0.2053571|  0.0580357|        0.0401786|   0.0178571|  0.0178571|  0.0178571|    0.0000000|     0.0044643|          33|                81|        21|     183|      25366|            476263|                  623909|          204413|       1420630|          570055|      1498|

Mapping
=======

Observations can be linked to shapefiles provided [on this page](https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/) (you will need to merge these two shapefiles: [here](http://osm13.openstreetmap.fr/~cquest/openfla/export/communes-20170111-shp.zip) and [here](http://osm13.openstreetmap.fr/~cquest/openfla/export/arrondissements-municipaux-20160128-shp.zip) since city-wide results for Paris, Lyon and Marseille are not provided) by merging the `code_commune_join` column of `results_fr_20170423` onto the `insee` column of the shapefile.
