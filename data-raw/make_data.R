## load the 6-digit soc 2010
soc2010_6digit <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_2010_6digit.csv")
save(soc2010_6digit, file="data/soc2010_6digit.rdata")
## load the complete soc 2010 coding system
soc2010_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc2010_all.csv")
save(soc2010_all, file="data/soc2010_all.rdata")
## load the complete soc 2018 coding system
soc2018_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc2018_all.csv")
save(soc2018_all, file="data/soc2018_all.rdata")
## load the 4-digit noc 2011 4digit
noc2011_4digit <- readr::read_csv("https://danielruss.github.io/codingsystems/noc_2011_4d.csv")
save(noc2011_4digit, file="data/noc2011_4digit.rdata")
## load the complete noc 2011 coding system
noc2011_all <- readr::read_csv("https://danielruss.github.io/codingsystems/noc2011_all.csv",col_types = "cccccccc")
save(noc2011_all, file="data/noc2011_all.rdata")
## load the complete soc1980 codingsystem
soc1980_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc1980_all.csv",col_types = "cccccccc")
save(soc1980_all, file="data/soc1980_all.rdata")
## load the most detailed soc1980 coding system
soc1980_detailed <- readr::read_csv("https://danielruss.github.io/codingsystems/soc1980_most_detailed.csv",col_types = "cccccc")
save(soc1980_detailed, file="data/soc1980_detailed.rdata")
## load the most detailed soc1980 coding system extended with zeros
soc1980_extended <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_1980_extended.csv",col_types = "cccccccc")
save(soc1980_extended, file="data/soc1980_extended.rdata")

