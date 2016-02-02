OUTFILE = gcdnum

.PHONY:			cleanout
.SILENT:		data

all:			data

data:			data/$(OUTFILE).rda

data/$(OUTFILE).rda:	data-raw/GCD.csv
			Rscript data-raw/GCD.R

cleanout:
			rm data/$(OUTFILE).rda
