message(".... building the soc2010 system ....")
## load the complete soc 2010 coding system
soc2010_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc2010_all.csv")
save(soc2010_all, file="data/soc2010_all.rda")

## load the 6-digit soc 2010
#soc2010_6digit <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_2010_6digit.csv")
soc2010_6digit <- soc2010_all |> dplyr::filter(Level==6) |> dplyr::select(code,title) |> codingsystem(name="soc2010 6-digit")
save(soc2010_6digit, file="data/soc2010_6digit.RData")
rm(soc2010_6digit)
rm(soc2010_all)

message(".... building the soc2018 system ....")
## load the complete soc 2018 coding system
soc2018_all <- codingsystem( readr::read_csv("https://danielruss.github.io/codingsystems/soc2018_all.csv"), name="soc2018")
save(soc2018_all, file="data/soc2018_all.rda")
rm(soc2018_all)

message(".... building the noc2011 system ....")
## load the complete noc 2011 coding system
noc2011_all <- codingsystem( readr::read_csv("https://danielruss.github.io/codingsystems/noc2011_all.csv",col_types = "cccccccc"), name="noc2011")
save(noc2011_all, file="data/noc2011_all.rda")

## load the 4-digit noc 2011 4digit
#noc2011_4digit <- readr::read_csv("https://danielruss.github.io/codingsystems/noc_2011_4d.csv")
noc2011_4digit <- noc2011_all |> dplyr::filter(Level==4) |> dplyr::select(code,title) |> codingsystem(name="noc2011 4-digit")
save(noc2011_4digit, file="data/noc2011_4digit.rda")
rm(noc2011_4digit)
rm(noc2011_all)

message(".... building the soc1980 system ....")
## load the complete soc1980 codingsystem
soc1980_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc1980_all.csv",col_types = "cccccccc")
save(soc1980_all, file="data/soc1980_all.rda")
rm(soc1980_all)

## load the most detailed soc1980 coding system
soc1980_detailed <- codingsystem( readr::read_csv("https://danielruss.github.io/codingsystems/soc1980_most_detailed.csv",col_types = "cccccc"), name="soc1980 most detailed")
save(soc1980_detailed, file="data/soc1980_detailed.rda")
rm(soc1980_detailed)

## load the most detailed soc1980 coding system extended with zeros
soc1980_extended <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_1980_extended.csv",col_types = "cccccccc")
save(soc1980_extended, file="data/soc1980_extended.rda")
rm(soc1980_extended)
message(".... finished ....")



