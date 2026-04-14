# install.packages("data.table")
library(data.table)

df_plane_data <- read.csv("data/plane-data.csv")

years <- 2000:2008

for (y in years) {
  file_path <- paste0("data/", y, ".csv.bz2")
  
  if (file.exists(file_path)) {
    assign(paste0("df", y), read.csv(file_path))
    assign(paste0("dt", y), data.table(get(paste0("df", y)))[, c("Year", "ArrDelay", "TailNum")])
  } else {
    message("Brak pliku: ", y)
  }
}

has_2000_2008 <- all(sapply(paste0("df", 2000:2008), exists))

if (has_2000_2008) {
  dtcombined <- rbindlist(list(dt2000, dt2001, dt2002, dt2003, dt2004, dt2005, dt2006, dt2007, dt2008))
} else {
  dtcombined <- dt2008
}

dtplanes <- data.table(df_plane_data)[, c("tailnum", "year")]
setnames(dtplanes, old = "tailnum", new = "TailNum")
setkey(dtplanes, TailNum)
MainData <- dtplanes[dtcombined, on = "TailNum"]
setnames(MainData, old = "year", new = "ProductionYear")
setnames(MainData, old = "Year", new = "FlightYear")
MainData <- na.omit(MainData)

setkey(MainData, ProductionYear)
MainData <- MainData[, .(MeanDelay = mean(ArrDelay)), by = .(ProductionYear, FlightYear)]


