#!/usr/bin/Rscript
##### R-Script to create gcd data for R package
##### Gerhard Nachtmann
##### 20160122-0321
##### 20170112
##### 20170209

library(data.table)
source("../R/left.R")

gcdnum <- fread("GCD.csv", encoding = "Latin-1")
gcdnum

gcdcols <- grep("GKZ", names(gcdnum), value = TRUE)
gcdnum[, (gcdcols) := lapply(.SD, left, 5), .SDcols = gcdcols]
gcdnum
names(gcdnum)
gcdnum[, POP20150101e := as.numeric(POP20150101e)]

### Some (5) are already duplicates in 2011
### reason: splitting of these municipalities
gcdnum[, GKZ11][duplicated(gcdnum[, GKZ11])]

### additionally 62205 Buch-St. Magdalena (60702 + 60734)
### and 61120 Trofaiach	(61102 + 61103 + 61117)
gcdnum[,GKZ13][duplicated(gcdnum[, GKZ13])]

## Splitting of municipalities in 2015 (GKZ11)
splitgem15 <- gcdnum[, GKZ11][duplicated(gcdnum[, GKZ11])]
splitgem15
# higher population first
setorder(gcdnum, GKZ11, GKZ12, GKZ13, -POP20150101e)
setkey(gcdnum, GKZ11, GKZ12, GKZ13)
nrow(gcdnum)
nrow(unique(gcdnum, by = key(gcdnum)))
gcdnum[splitgem15]
unique(gcdnum, by = key(gcdnum))[splitgem15]
### take the new gcd for the *bigger* parts (by population)
### of the gcd before splitting to be unique
gcdnum <- unique(gcdnum, by = key(gcdnum))
nrow(gcdnum)
gcdnum[splitgem15]

# ### alternative way to delete POP20150101e -- outdated (col nr)
# names(gcdnum)
# set(gcdnum, j = 6L, value = NULL)

# g17 <- fread("gem2017.csv", colClasses = "character",
#             key = "GKZ16")
# setkeyv(gcdnum, key(g17))
# gcdnum[g17, GKZ17 := GKZ17]

g17 <- fread("gem2017.csv", colClasses = "character")
# gcdnum[g17, GKZ17 := GKZ17, on = "GKZ16"]
gcdnum[g17, GKZ17 := GKZ17, on = .(GKZ16)] # same as above
gcdnum[NA_character_, GKZ17 := GKZ16, on = "GKZ17"]
gcdnum[GKZ16 != GKZ17, ] # ok

# gcdnum <- as.data.frame(gcdnum)

if(!dir.exists("../data")){dir.create("../data")}

# write.csv(gcdnum, "../data/gcdnum.csv", row.names = FALSE)
# save(gcdnum, file = "../data/gcdnum.rda", compress = "xz")
# save(gcdnum, file = "data/gcdnum.rda", compress = "xz")

devtools::use_data(gcdnum, compress = "xz", overwrite = TRUE)
