 library(data.table)
##graphics.off()

library(Rcpp)
#library(fpc)

sourceCpp("path.cpp")

if(!exists("pl")) {
    pl = readRDS("gpsl_new.rds")
}

kv = c(308, 78, 165, 452, 21, 106, 8, 30, 639, 456, 53, 484, 453)
kv = 10*kv
##kv = rep(10,13)


make_paths = function(p, k) {
    p <- p[,1:2]
    
    #cl <- kmeans(p, centers=k, algorithm="MacQueen", iter=50)
    cl <- kmeans(p, centers=k, nstart=2)
    centers <- cl[["centers"]]
    
    x <- as.vector(centers[,1])
    y <- as.vector(centers[,2])
    path <- c(x,y)
    idx <- find_path(path) + 1
    dt <- data.table(gps_long= x[idx], gps_lat = y[idx])
}

make_paths2 = function(p, k) {
    print(k)
    p <- p[,1:2]
    
    cl <- kmeans(p, centers=k)
    #cl <- kmeans(p, centers=k)
    centers <- cl[["centers"]]
    
    x <- as.vector(centers[,1])
    y <- as.vector(centers[,2])
    path <- c(x,y)
    return(path)
}

paths = lapply(p, make_paths)

myplot = function(p) {
    plot(p)
    lines(p)
}

par(mfrow=c(4,4))
lapply(paths, myplot)
