## load the 6-digit soc 2010
soc2010_6digit <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_2010_6digit.csv")
save(soc2010_6digit, file="data/soc2010_6digit.rdata")
## load the 4-digit noc 2011
noc2011_4digit <- readr::read_csv("https://danielruss.github.io/codingsystems/noc_2011_4d.csv")
save(noc2011_4digit, file="data/noc2011_4digit.rdata")
