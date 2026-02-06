# setwd("~/Dane")
df_plane_data <- read.csv("plane-data.csv")
df2000 <- read.csv("2000.csv.bz2")
df2001 <- read.csv("2001.csv.bz2")
df2002 <- read.csv("2002.csv.bz2")
df2003 <- read.csv("2003.csv.bz2")
df2004 <- read.csv("2004.csv.bz2")
df2005 <- read.csv("2005.csv.bz2")
df2006 <- read.csv("2006.csv.bz2")
df2007 <- read.csv("2007.csv.bz2")
df2008 <- read.csv("2008.csv.bz2")

install.packages("data.table")
library(data.table)

dt2000 <- data.table(df2000)[, c("Year", "ArrDelay", "TailNum")]
dt2001 <- data.table(df2001)[, c("Year", "ArrDelay", "TailNum")]
dt2002 <- data.table(df2002)[, c("Year", "ArrDelay", "TailNum")]
dt2003 <- data.table(df2003)[, c("Year", "ArrDelay", "TailNum")]
dt2004 <- data.table(df2004)[, c("Year", "ArrDelay", "TailNum")]
dt2005 <- data.table(df2005)[, c("Year", "ArrDelay", "TailNum")]
dt2006 <- data.table(df2006)[, c("Year", "ArrDelay", "TailNum")]
dt2007 <- data.table(df2007)[, c("Year", "ArrDelay", "TailNum")]
dt2008 <- data.table(df2008)[, c("Year", "ArrDelay", "TailNum")]

dtcombined <- rbindlist(list(dt2000, dt2001, dt2002, dt2003, dt2004, dt2005, dt2006, dt2007, dt2008))


dtplanes <- data.table(df_plane_data)[, c("tailnum", "year")]
setnames(dtplanes, old = "tailnum", new = "TailNum")
setkey(dtplanes, TailNum)
MainData <- dtplanes[dtcombined, on = "TailNum"]
setnames(MainData, old = "year", new = "ProductionYear")
setnames(MainData, old = "Year", new = "FlightYear")
MainData <- na.omit(MainData)

setkey(MainData, ProductionYear)
MainData <- MainData[, .(MeanDelay = mean(ArrDelay)), by = .(ProductionYear, FlightYear)]


