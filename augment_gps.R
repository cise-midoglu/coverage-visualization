source("util.R")

find_interval2 = function(point) {
    n = nrow(path) -1    
    rmin = 10000
    idx = -1
    for(i in 1:n) {
        p1 = pathM[i,]
        p2 = pathM[i+1,]
        R = haversine_dist2(p1,p2)
        r1 = haversine_dist2(point,p1)
        r2 = haversine_dist2(point,p2)
        r = r1 + r2

        if(r < rmin) {
            rmin = r
            idx = i
        }
    }
    cnt <<- cnt + 1
    if(cnt%%100 == 0) {
        printf("row %d %.2f %%\n", cnt, 100*cnt/nrow)
    }

    idx
}

##for(i in seq_along(path)) {
gpsi = list()

##for(i in 1:13) {
for(i in 1:2) {
    cnt = 1
    path = paths[[i]]
    gps = gpsl[[i]]

    gps2 = gps[,1:2]
    gpsM = as.matrix(gps2)

    #path = path[complete.cases(path)]

    pathM = as.matrix(path)
    nrow = nrow(gpsM)
    printf("\t path %d, nrow %d\n", i, nrow)
    idx = apply(gpsM, 1, find_interval2)
    ##idx = apply(gpsM, 1, find_interval2)
    gps3 = cbind(gps, segment=idx) 
    gpsi[[i]] = gps3
}
