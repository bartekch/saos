s <- search_judgments(limit = 3000, force = TRUE)
s <- c(s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s,s)
s <- split(s, rep(1:(length(s)/100), each = 100))

lol1 <- function(s){
  r <- s[[1]]
  #print(tracemem(r[[1]]))
  for (i in 2:length(s)){
    r <- c(r, s[[i]])
    #print(tracemem(r[[1]]))
  }
  r
}

lol2 <- function(s){
  r <- vector("list", 1500)
  r[1:100] <- s[[1]]
  for (i in 1:(length(s)-1)){
    r[(1+100*i):((1+i)*100)] <- s[[i+1]]
  }
  r
}

library(microbenchmark)
microbenchmark(lol1(s), lol2(s))
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# lol1(s) 229.3368 237.4230 271.2018 286.2770 291.7374 328.0181   100
# lol2(s) 136.1824 140.0892 164.2576 145.1065 193.8357 221.9897   100

# the second function is considerably faster


# old version
system.time(invisible(search_judgments(limit = 20000, force = TRUE, verbose = FALSE)))
# użytkownik     system   upłynęło 
# 8.412      0.616    721.332

# new version
system.time(invisible(search_judgments(limit = 20000, force = TRUE, verbose = FALSE)))
# użytkownik     system   upłynęło 
# 10.606      0.522    689.329