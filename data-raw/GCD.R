#!/usr/bin/Rscript
##### R-Script to create gcd data for R package
##### Gerhard Nachtmann
##### 20160122

library(data.table)
source("./R/left.R")

gcdraw <- fread("data-raw/GCD.csv", encoding = "Latin-1")
gcdraw

gcdnum <- gcdraw[, lapply(.SD, left, 5)]
gcdnum
names(gcdnum)
gcdnum[, POP20150101e := as.numeric(POP20150101e)]

### Some (5) are already duplicates in 2011
### reason: splitting of these municipalities
gcdraw[, GKZ11][duplicated(gcdraw[, GKZ11])]

### additionally 62205 Buch-St. Magdalena (60702 + 60734)
### and 61120 Trofaiach	(61102 + 61103 + 61117)
gcdraw[,GKZ13][duplicated(gcdraw[, GKZ13])]

## Splitting of municipalities in 2015 (GKZ11)
splitgem15 <- gcdnum[, GKZ11][duplicated(gcdnum[, GKZ11])]
splitgem15
# higher population first
setorder(gcdnum, GKZ11, GKZ12, GKZ13, -POP20150101e)
setkey(gcdnum, GKZ11, GKZ12, GKZ13)
nrow(gcdnum)
nrow(unique(gcdnum))
gcdnum[splitgem15]
unique(gcdnum)[splitgem15]
### take the new gcd for the *bigger* parts (by population)
### of the gcd before splitting to be unique
gcdnum <- unique(gcdnum)
nrow(gcdnum)
gcdnum[splitgem15]

# ### alternative way to delete POP20150101e
# names(gcdnum)
# set(gcdnum, j = 6L, value = NULL)

# gcdnum <- as.data.frame(gcdnum)
# write.csv(data/gcdnum, "gcdnum.csv", row.names = FALSE)
save(gcdnum, file = "data/gcdnum.rda", compress = "xz")