require(sp)
require(rgdal)
require(raster)
require(spdep)

# Source https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/
fr_adm17 <- readOGR(dsn = "/Users/francesco/public_git/french_pres_election_17/communes-20170112",
                    layer = "communes-20170112-arrond",
                    stringsAsFactors = F)

# One duplicated row
fr_adm17 <- fr_adm17[-which(fr_adm17$nom == "Annecy" & fr_adm17$surf_ha == 1599),]

# Create neigh
nb_50km <- dnearneigh(coordinates(fr_adm17),  d1=0, d2=50, longlat=T, row.names=fr_adm17$insee)
save(nb_50km, file = "/Users/francesco/public_git/french_pres_election_17/communes-20170112-arrond_nb_50km.RData")

# Calculate demographics
library(DBI)
library(RPostgreSQL)

conn <- dbConnect(
  drv = PostgreSQL(),
  dbname = "insee_com2013",
  host = "localhost",
  port = "5432",
  user = "francesco",
  password = "")

retraites_IMG2A_count <- dbGetQuery(conn, "select * from fr_com2017.retraites_IMG2A_count")
chomeurs_count <- dbGetQuery(conn, "select * from fr_com2017.chomeurs_count")
actifs_count <- dbGetQuery(conn, "select * from fr_com2017.actifs_count")
immigrants_count <- dbGetQuery(conn, "select * from fr_com2017.immigrants_count")

demo_dat <- merge(immigrants_count, retraites_IMG2A_count)
demo_dat <- merge(demo_dat, chomeurs_count)
demo_dat <- merge(demo_dat, actifs_count)
demo_dat$nb_index <- match(demo_dat$insee, 
                           attr(nb_50km, 'region.id'))

demo_vars <- names(demo_dat)[2:5]

for (v in demo_vars) {
  demo_dat[[paste0(v, "_50km")]] <- NA
  for(i in demo_dat$nb_index) {
    print(i)
    demo_dat[[paste0(v, "_50km")]][which(demo_dat$nb_index == i)] <- sum(demo_dat[[v]][nb_50km[[i]]])
  }
}

# Merge with first round
## Source
load("~/public_git/french_pres_election_17/results_fr_20170423.RData")
results_fr_20170423[,6:22] <- apply(results_fr_20170423[,6:22], 2,
                                    FUN = function(x) as.numeric(gsub("[^0-9]", "", x)))

## Add perc
for (n in colnames(results_fr_20170423)[6:16]) {
  results_fr_20170423[[paste0(n, " %")]] <- results_fr_20170423[[n]] / results_fr_20170423[['Exprimés']]
}

results_fr_20170423 <- results_fr_20170423[!duplicated(results_fr_20170423$code_commune),]
results_fr_20170423$code_commune_join <- gsub("^0", "", results_fr_20170423$code_commune)

results_fr_20170423_wt_demog <- merge(results_fr_20170423, demo_dat, by.y = 'insee', by.x = 'code_commune_join')

# Add inscrits in neigh
results_fr_20170423_wt_demog$nb_index <- 
  match(results_fr_20170423_wt_demog$code_commune_join, 
        attr(nb_50km, 'region.id'))
results_fr_20170423_wt_demog[["inscrits_50km"]] <- NA

for(i in results_fr_20170423_wt_demog$nb_index) {
  print(i)
  results_fr_20170423_wt_demog[["inscrits_50km"]][which(results_fr_20170423_wt_demog$nb_index == i)] <- 
    sum(results_fr_20170423_wt_demog[['Inscrits']][results_fr_20170423_wt_demog$nb_index %in% nb_50km[[i]]])
}

# Add density
results_fr_20170423_wt_demog <- merge(results_fr_20170423_wt_demog, 
                                      as.data.frame(fr_adm17)[,c('insee','surf_ha')],
                                      by.x= 'code_commune_join',
                                      by.y='insee',
                                      all.x =T)


# Add revenue

library(readxl)
revenue_data_insee <- read_excel("~/public_git/french_pres_election_17/revenue_data_insee.xlsx")

results_fr_20170423_wt_demog <- 
  merge(results_fr_20170423_wt_demog, revenue_data_insee[,c('COM','RFPQ111','RFPQ211','RFPD111','RFPMO11')],
        by.x = 'code_commune_join', by.y = "COM", all.x = TRUE)

names(results_fr_20170423_wt_demog)[46:49] <- c('revenue_1qrt', 'revenue_median', 'revenue_1dec', 'revenue_mean')

save(results_fr_20170423_wt_demog, file = "/Users/francesco/public_git/french_pres_election_17/results_fr_20170423_wt_demog.RData")
write.csv(results_fr_20170423_wt_demog, file = '/Users/francesco/public_git/french_pres_election_17/results_fr_20170423_wt_demog.csv')


# Merge with second round
load("~/public_git/french_pres_election_17/results_fr_20170507.RData")


