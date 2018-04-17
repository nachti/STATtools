# STATtools

[![Travis-CI Build Status](https://travis-ci.org/nachti/STATtools.svg?branch=master)](https://travis-ci.org/nachti/STATtools)

Tools of Statistics Austria (STAT)

## Installation

Development version from github:

```R
# install.packages("devtools")
devtools::install_github("nachti/STATtools")
```

## NEWS

### 0.1.4 2017-02-09
Bugfix: took latest targetyear instead of `targetyear` argument
given in `gcdrc()`.
Fixed troubles due to duplicates in `gcdnum` caused by new
version of `data.table` (unique needs `by` now) ...

### 0.1.3 2017-01-13
Updated due to disintegration of district Wien Umgebung 20170101
Changed `.Rbuildignore` (included `Makevars` and `data-raw`),
because `Makevars` did not work any more ...


### 0.1.2 2016-03-21
added GKZ (Gemeindekennziffer) 90001 for Vienna
`data.table` dependency just for building `gcdnum.rda` from
`GCD.csv`, which is not very well structured

### 0.1.1 2016-02-11
First release
