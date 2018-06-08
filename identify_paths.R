library(data.table)
if(!exists("gps")) {
    gps <- readRDS("gps_new.rds")
}

cuts = list()

##1
##c = gps$gps_long>10.6 & gps$gps_lat<59.9036 
c1 = gps$gps_long>10.6 & gps$gps_lat<59.4
c2 = gps$gps_long>10.6 & gps$gps_long<10.84 & gps$gps_lat<59.9036 & gps$gps_lat>59.72 
c3 = gps$gps_long>10.6 & gps$gps_long<10.84 & gps$gps_lat<59.72   & gps$gps_lat>59.35 
c = c1 | c2 | c3
cuts[[length(cuts)+1]] = c

##2
c1 = gps$gps_long>10.24 & gps$gps_lat>60.16935 & gps$gps_long<11 & gps$gps_lat<60.4
c2 = gps$gps_long>10.6 & gps$gps_lat<=60.16935 & gps$gps_long<10.88 & gps$gps_lat>59.95
##c3 = gps$gps_long>10.6 & gps$gps_lat<=60.16935 & gps$gps_long<10.88 & gps$gps_lat>59.95
c = c1 | c2
cuts[[length(cuts)+1]] = c

##3
c1 = gps$gps_lat<59.735 & gps$gps_long<10.5 & gps$gps_long>10
c2 = gps$gps_long>9.34 & gps$gps_long<=10 & gps$gps_lat<59.4
##c2 = gps$gps_long>9.4 & gps$gps_long<=10 & gps$gps_lat<59.4
c = c1 | c2
cuts[[length(cuts)+1]] = c

##4
c1 = gps$gps_long>11.25 & gps$gps_lat<60.3 & gps$gps_lat>59.8
c2 = gps$gps_long>11.15 & gps$gps_long<=11.25 & gps$gps_lat<60.0 & gps$gps_lat>59.8
c3 = gps$gps_long>11.05 & gps$gps_long<=11.15 & gps$gps_lat<59.9542 & gps$gps_lat>59.8
c4 = gps$gps_long>10.292 & gps$gps_lat>61.5 & gps$gps_long<12 & gps$gps_lat<63.15
c5 = gps$gps_long>10.95 & gps$gps_lat<61.5 & gps$gps_long<11.57 & gps$gps_lat>60.8795
c6 = gps$gps_long>11.545 & gps$gps_lat<60.87 & gps$gps_lat>60.3
c = c1 | c2 | c3 | c4 | c5 | c6
cuts[[length(cuts)+1]] = c

##5
c1 = gps$gps_long>11.16 & gps$gps_long<11.25 & gps$gps_lat<60.25 & gps$gps_lat>60
c2 = gps$gps_long>11.2 & gps$gps_long<11.23 & gps$gps_lat>=60.25 & gps$gps_lat<60.3
c = c1 | c2
cuts[[length(cuts)+1]] = c

##6
c1 = gps$gps_long<9.05 & gps$gps_lat>62.07 & gps$gps_lat<63
c2 = gps$gps_long>=9.05 & gps$gps_long<9.125 & gps$gps_lat>62.09 & gps$gps_lat<62.1
c = c1 | c2
cuts[[length(cuts)+1]] = c

##7
c = gps$gps_long>9.25 & gps$gps_long<9.338 & gps$gps_lat>59.5 & gps$gps_lat<60
cuts[[length(cuts)+1]] = c

##8
c1 = gps$gps_long<11.54 & gps$gps_long>11.11 & gps$gps_lat>60.77 & gps$gps_lat<60.88
c2 = gps$gps_long>=11.54 & gps$gps_long<11.547 & gps$gps_lat>60.877 & gps$gps_lat<60.88
c = c1 | c2
cuts[[length(cuts)+1]] = c

##9
c = gps$gps_lat>63.1 & gps$gps_long>10.41
cuts[[length(cuts)+1]] = c

##10
c = gps$gps_lat<59.773 & gps$gps_long<9.9
c = c & !(cuts[[3]] | cuts[[7]])
cuts[[length(cuts)+1]] = c

##11
c = gps$gps_long>10.836 & gps$gps_lat<59.72 & gps$gps_lat>59.4
cuts[[length(cuts)+1]] = c

##12
c1 = gps$gps_lat>60.8
c2 = gps$gps_lat<=60.8 & gps$gps_long>11 & gps$gps_lat>59.5
c3 = gps$gps_long>10.8 & gps$gps_long<11.05 & gps$gps_lat<60.0 & gps$gps_lat>59.9
c = c1 | c2 | c3
c = c & !(cuts[[4]] | cuts[[5]] | cuts[[6]] | cuts[[8]] | cuts[[9]] | cuts[[11]])
cuts[[length(cuts)+1]] = c

##13
c = cuts[[1]]
for(x in cuts) c = c | x
c = !c
cuts[[length(cuts)+1]] = c

p = lapply(cuts, function(cut) { gps[cut]})

p[[12]] = p[[12]][!(gps_long>11.5 & gps_lat<62)]

##par(mfrow=c(3,4))

for(i in seq_along(p)) {
    ##plot(p[[i]][,1:2])
    ##plot(p[[i]][,1:2], xlim = c(5,16), ylim=c(56, 68))
}

##saveRDS(p1, "gps1.rds")
##saveRDS(p2, "gps2.rds")
##saveRDS(p3, "gps3.rds")
##saveRDS(p4, "gps4.rds")
##saveRDS(p5, "gps5.rds")

##last path
##gps_long>11.12 & gps_long<11.54 & gps_lat<60.88 & gps_lat>60.77

#Oslo station path hacks

#path2 fix
#c2 = gps$gps_long>10.78 & gps$gps_lat<59.95 & gps$gps_long<10.8 & gps$gps_lat>59.908

#path12 fix
#c3 = gps$gps_long>10.787 & gps$gps_lat<59.907 & gps$gps_long<10.8 & gps$gps_lat>59.905
