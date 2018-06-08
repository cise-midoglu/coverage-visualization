library(rgl)
library(anytime)
library(data.table)
printf <- function(...) cat(sprintf(...))

make_r <- function(path) {
    r <- vector()
    for(i in 1:nrow(path)) {
        p1 <- as.numeric(path[i,])
        p2 <- as.numeric(path[i+1,])
        dp <- p1 - p2
        r[i] <- haversine_dist2(p1, p2);
        #r[i] = sqrt(sum(dp*dp))        
    }
    r
}

mkrdr <- function(DT) {
    r <- vector()
    for(i in 1:nrow(gps)) {
        p1 <- as.numeric(gps[1,1:2])
        p2 <- as.numeric(gps[i,1:2])
        r[i] <- haversine_dist2(p1,p2)
    }
    
    dr <- abs(c(NA, diff(r)))
    DT <- cbind(DT, "r" = r)
    DT <- cbind(DT, "dr" = dr)
    return(DT)
}

haversine_dist2 <- function(p1, p2) {
    R <- 6371
    TO_RAD <- 3.1415926536/180
    ph1 <- p1[1]; ph2 <- p2[1]
    th1 <- p1[2]; th2 <- p2[2]   

    ph1 = ph1 - ph2;
    ph1 = ph1*TO_RAD; th1 = th1*TO_RAD; th2 = th2*TO_RAD;
    
    dz = sin(th1) - sin(th2);
    dx = cos(ph1) * cos(th1) - cos(th2);
    dy = sin(ph1) * cos(th1);
    asin(sqrt(dx * dx + dy * dy + dz * dz) / 2) * 2 * R;
}

fraction_circle <- function(p1, p2, f) {
    R <- 6371
    TO_RAD <- 3.1415926536/180
    d <- haversine_dist2(p1,p2)
    delta <- d/R

    ph1 <- p1[1]; ph2 <- p2[1]
    th1 <- p1[2]; th2 <- p2[2]   

    ph1 = ph1*TO_RAD; ph2 = ph2*TO_RAD; th1 = th1*TO_RAD; th2 = th2*TO_RAD;
    #printf("%f %f %f %f\n", ph1, ph2, th1, th2)
    a = sin((1-f)*delta)/sin(delta)
    b = sin(f*delta)/sin(delta)
    x = a*cos(ph1)*cos(th1) + b*cos(ph2)*cos(th2)
    y = a*cos(ph1)*sin(th1) + b*cos(ph2)*sin(th2)
    z = a*sin(ph1) + b*sin(ph2)
    #printf("%f %f %f\n", x, y, z)
    phi = atan2(z, sqrt(x*x + y*y))
    thi = atan2(y, x)
    c(phi/TO_RAD, thi/TO_RAD)
}

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
