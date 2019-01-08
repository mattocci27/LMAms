# simulate covariate data
set.seed(2017)
n <- 50
sdx <- 6
sdobs <- 5
taux <- 1 / (sdobs * sdobs)
truex <- rnorm(n, 0, sdx)
errorx <- rnorm(n, 0, sdobs)
obsx <- truex + errorx

# simulate response data
alpha <- 0
beta <- 10
sdy <- 20
errory <- rnorm(n, 0, sdy)
obsy <- alpha + beta*truex + errory
parms <- data.frame(alpha, beta)

temp <- data_frame(obsx, obsy, truex) %>%
  gather(obs, val, c(obsx, truex))

ggplot(temp, aes(x = val, y = obsy, col = obs)) +
  geom_point() +
  theme_light() +
  geom_abline(slope=l1[[1]][2], intercept=l1[[1]][1], lty = 1, col = "red") +
  geom_abline(slope=l2[[1]][2], intercept=l2[[1]][1], lty = 2, col = "blue") 


l1 <- lm(obsy ~ obsx)
l2 <- lm(obsy ~ truex) 

sma(obsy ~ obsx) %>% summary
sma(obsy ~ truex) %>% summary

