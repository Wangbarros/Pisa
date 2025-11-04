setwd("~/Dropbox/PostDoc/Pisa")

df2022 = read.csv("data/df2022.csv")

unique(df2022$CNT)

#filter out FL and WB

df2022_filtered  = df2022[df2022$CNT %in% c("ARG", "AUS", "BRA", "KOR","JAP","ESP", "FIN","MEX",
                                               "POL","QAT" ,"SWE" ,"USA"),]

write.csv(df2022_filtered,'data/df2022_filtered.csv', row.names = FALSE)

write.csv(df2022,'data/df2022_2.csv', row.names = FALSE)

df2022_2 = read.csv("data/df2022_2.csv")
