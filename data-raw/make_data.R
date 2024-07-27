message(".... building the soc2010 system ....")
## load the complete soc 2010 coding system
soc2010_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc2010_all.csv")
use_data(soc2010_all,overwrite = TRUE)
#save(soc2010_all, file="data/soc2010_all.rda")

## load the 6-digit soc 2010
#soc2010_6digit <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_2010_6digit.csv")
soc2010_6digit <- soc2010_all |> dplyr::filter(Level==6) |> dplyr::select(code,title)
use_data(soc2010_6digit,overwrite = TRUE)
#save(soc2010_6digit, file="data/soc2010_6digit.RData")
rm(soc2010_6digit)
rm(soc2010_all)

message(".... building the soc2018 system ....")
## load the complete soc 2018 coding system
soc2018_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc2018_all.csv")
use_data(soc2018_all,overwrite = TRUE)
#save(soc2018_all, file="data/soc2018_all.rda")
rm(soc2018_all)

message(".... building the noc2011 system ....")
## load the complete noc 2011 coding system
noc2011_all <- readr::read_csv("https://danielruss.github.io/codingsystems/noc2011_all.csv",col_types = "cccccccc")
#save(noc2011_all, file="data/noc2011_all.rda")
use_data(noc2011_all,overwrite = TRUE)

## load the 4-digit noc 2011 4digit
#noc2011_4digit <- readr::read_csv("https://danielruss.github.io/codingsystems/noc_2011_4d.csv")
noc2011_4digit <- noc2011_all |> dplyr::filter(Level==4) |> dplyr::select(code,title)
#save(noc2011_4digit, file="data/noc2011_4digit.rda")
use_data(noc2011_4digit,overwrite = TRUE)
rm(noc2011_4digit)
rm(noc2011_all)

message(".... building the soc1980 system ....")
## load the complete soc1980 codingsystem
soc1980_all <- readr::read_csv("https://danielruss.github.io/codingsystems/soc1980_all.csv",col_types = "cccccccc")
#save(soc1980_all, file="data/soc1980_all.rda")
use_data(soc1980_all,overwrite = TRUE)
rm(soc1980_all)

## load the most detailed soc1980 coding system
soc1980_detailed <- readr::read_csv("https://danielruss.github.io/codingsystems/soc1980_most_detailed.csv",col_types = "cccccc")
#save(soc1980_detailed, file="data/soc1980_detailed.rda")
use_data(soc1980_detailed,overwrite = TRUE)
use_data(soc1980_detailed,internal = TRUE,overwrite = TRUE)
#rm(soc1980_detailed)

## load the most detailed soc1980 coding system extended with zeros
soc1980_extended <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_1980_extended.csv",col_types = "cccccccc")
#save(soc1980_extended, file="data/soc1980_extended.rda")
use_data(soc1980_extended,overwrite = TRUE)
rm(soc1980_extended)
message(".... finished ....")



