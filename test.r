aa <- rnorm(100)
bb <- rnorm(100)
cc <- rnorm(100)

m <- cbind(aa, bb, cc, cc)
cov(m)

cov2cor(aa,bb,cc)
