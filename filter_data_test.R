#setwd("~/Dropbox/PostDoc/Pisa")

df2022 = read.csv("data/df2022.csv")
df2018 = read.csv("data/df2018.csv")
df2015 = read.csv("data/df2015.csv")
df2012 = read.csv("data/df2012.csv")
df2009 = read.csv("data/df2009.csv")
df2006 = read.csv("data/df2006.csv")
df2003 = read.csv("data/df2003.csv")

unique(df2022$CNT)
c_list = c("AUS", "BRA", "KOR","ESP", "FIN","MEX",
           "POL" ,"SWE" ,"USA")

#filter out FL and WB

df2022_filtered  = df2022[df2022$CNT %in% c(c_list),]
write.csv(df2022_filtered,'data/df2022_filtered.csv', row.names = FALSE)

df2018_filtered  = df2018[df2018$CNT %in% c(c_list),]
write.csv(df2022_filtered,'data/df2018_filtered.csv', row.names = FALSE)

df2015_filtered  = df2015[df2015$CNT %in% c(c_list),]
write.csv(df2022_filtered,'data/df2015_filtered.csv', row.names = FALSE)

df2012_filtered  = df2012[df2012$CNT %in% c(c_list),]
write.csv(df2022_filtered,'data/df2012_filtered.csv', row.names = FALSE)

df2009_filtered  = df2009[df2009$CNT %in% c(c_list),]
write.csv(df2022_filtered,'data/df2009_filtered.csv', row.names = FALSE)

df2006_filtered  = df2006[df2006$CNT %in% c(c_list),]
write.csv(df2022_filtered,'data/df2006_filtered.csv', row.names = FALSE)

df2003_filtered  = df2003[df2003$CNT %in% c(c_list),]
write.csv(df2022_filtered,'data/df2003_filtered.csv', row.names = FALSE)




