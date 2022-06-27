## load the 6-digit soc 2010
soc2010_6digit <- readr::read_csv("https://danielruss.github.io/codingsystems/soc_2010_6digit.csv")
save(soc2010_6digit, file="data/soc2010_6digit.rdata")
## load the 4-digit noc 2011
noc2011_4digit <- readr::read_csv("https://danielruss.github.io/codingsystems/noc_2011_4d.csv")
save(noc2011_4digit, file="data/noc2011_4digit.rdata")
## load the complete soc1980 codingsystem
getSoc1980Code <- function(division,major,minor,unit,title){
  x<-c(division,major,minor,unit)
  x <- x[!is.na(x)]
  return(x[length(x)])
}
soc1980 <- read_csv("https://danielruss.github.io/codingsystems/soc1980.csv",col_types = "ccccc") %>% mutate(code=pmap_chr(.,getSoc1980Code))
