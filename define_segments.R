#library(fields)
#require2(geosphere)

source("util.R")

get_total_dist <- function(path) {
    dist_tot = 0
    n <- nrow(path) - 1
    for(i in 1:n) {
        p1 <- as.numeric(path[i,])
        p2 <- as.numeric(path[i+1,])
        r <- haversine_dist2(p1, p2);
        dist_tot = dist_tot + r        
    }
    dist_tot
}

get_total_dist2 <- function(path) {
    dist_tot = 0
    n <- nrow(path) - 1
    for(i in 1:n) {
        p1 <- as.numeric(path[i,])
        p2 <- as.numeric(path[i+1,])
        dp <- p1 - p2
        r = sqrt(sum(dp*dp))
        dist_tot = dist_tot + r        
    }
    dist_tot
}

get_polar <- function(path, i) {
    p1 <- as.numeric(path[i,])
    p2 <- as.numeric(path[i+1,])
    dp <- p2 - p1
    r <- sqrt(sum(dp*dp))
    theta <- atan2(dp[2],dp[1])
    c(r,theta)
}

get_vec <- function(p) {
    c(p[1]*cos(p[2]), p[1]*sin(p[2]))
}

rebin <- function(path, w) {
    n <- nrow(path) - 1
    path2 <- path[1,]

    dr <- w
    r <- 0

    for(i in 1:n) {
        point <- path2[1,]
        point <- point - point
        p1 <- as.numeric(path[i+0,])
        p2 <- as.numeric(path[i+1,])
        dr <- dr - r
        r <- haversine_dist2(p1,p2)       
        while(dr<r) {
            p1 <- fraction_circle(p1, p2, dr/r)
            row <- p1
            path2 <- rbind(path2, point + row)
            r <- r - dr
            dr <- w
        }
    }

    p1 <- as.numeric(path2[nrow(path2),])
    p2 <- as.numeric(path[nrow(path)-0,])    
    r <- haversine_dist2(p1,p2)

    p1 <- fraction_circle(p1, p2, 1.0)
    path2 <- rbind(path2, point + p1)
}

resample = function(path) {
    segment_size = 5.0
    dist_tot <- get_total_dist(path)
    
    bins <- as.integer(dist_tot/segment_size)
    w <- dist_tot/bins
    print(dist_tot)
    print(w)

    path2 <- rebin(path, w)
}

par(mfrow=c(2,1))

p = readRDS("paths.rds")

myplot = function(p) {
    plot(p)
    lines(p)
}

#plot(p[[1]], xlim = c(5,12), ylim=c(58, 64))
plot(p[[1]])
lines(p[[1]])

for(i in 2:length(p)) {
    lines(p[[i]])
    points(p[[i]])
}


pn = lapply(p, resample)

##plot(pn[[1]], xlim = c(5,12), ylim=c(58, 64))
plot(pn[[1]])
lines(pn[[1]])
for(i in 2:length(pn)) {
    lines(pn[[i]])
    points(pn[[i]])
}

##xsaveRDS(pn,"paths_fix.rds")
