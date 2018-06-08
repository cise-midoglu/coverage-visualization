library(rgl)
source("util.R")
graphics.off()

##rolll_24201 = readRDS("segment1/metaf_24201.rds")
##rolll_24202 = readRDS("segment1/metaf_24202.rds")
##rolll_24214 = readRDS("segment1/metaf_24214.rds")

rolll_24201 = readRDS("metaf_24201.rds")
rolll_24202 = readRDS("metaf_24202.rds")
rolll_24214 = readRDS("metaf_24214.rds")

t0 = 1500970710

##rolll_24201 = lapply(rolll_24201, function(dt) dt[timestamp<t0])

paths = readRDS("paths_fix.rds")

##roll = rolll[[1]]
##path = paths[[11]]

asdf = function(roll, path) {
    path[, segment := seq_len(.N)]
    N = roll[,.N, by=segment]$N
    ##bysegment = roll[,getmode(devicemode), by=segment]
    bysegment = roll[,max(devicemode), by=segment]
    bysegment = cbind(bysegment, N=N)

    setkey(path, segment)
    setkey(bysegment, segment)
    path2 = path[bysegment, roll="nearest"]
}

paths2_24201 = Map(asdf, rolll_24201, paths)
saveRDS(paths2_24201, "mode_24201.rds")
paths2_24202 = Map(asdf, rolll_24202, paths)
saveRDS(paths2_24202, "mode_24202.rds")
paths2_24214 = Map(asdf, rolll_24214, paths)
saveRDS(paths2_24214, "mode_24214.rds")


##plot(x=path2$gps_long, y=path2$gps_lat, col=path2$V1+1)

