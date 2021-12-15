

library(BayesFactor)


par(mfrow = c(1,2))


getP <- function(n=10, effectSize = 0, sd = 1){
  x = rnorm(n, mean = effectSize, sd= sd) 
  return(t.test(x)$p.value)
}


plot(NULL, xlim = c(0,1), ylim = c(0,3), xlab = "p-value", ylab = "probabilty of observing p", main = "Expected p distribution")

abline(h=1)

alt1 = replicate(10000, getP(n=2, effectSize = 0.5))
x = hist(alt1, breaks = 20, plot = F)
lines(x$mids, x$density, type = "both", pch = 1, col = "blue")


alt2 = replicate(10000, getP(n=20, effectSize = 0.5))
x = hist(alt2, breaks = 20, plot = F)
lines(x$mids, x$density, type = "both", pch = 2, col = "red")


legend("topright", legend = c("null", "underpowered", "properly powered"), pch = 1, col = c("black", "blue", "red"))



getResult <- function(n=10, effectSize = 0.4, sd = 1){
  x = rnorm(n, mean = effectSize, sd= sd) 
  p = t.test(x)$p.value
  lmod = lm(x ~ 1)
  #lmod2 = lm(x ~ 0)
  #anova(lmod, lmod2)
  BF = extractBF(ttestBF(x))$bf
  odds = BF/(BF + 1)
  return(c(p = p, oddsAlt = odds))
}

sim1 = as.data.frame(t(replicate(500, getResult(n = 2))))
sim2 = as.data.frame(t(replicate(500, getResult(n = 15))))
sim3 = as.data.frame(t(replicate(500, getResult(n = 100))))


plot(oddsAlt ~ p, data = sim1, ylim = c(0,1), xlim = c(0,1.1), ylab = "Odds in favor of the alternative", xlab = "p-value", main = "BF vs. p-value")
points(oddsAlt ~ p, data = sim2, col = "red")
points(oddsAlt ~ p, data = sim3, col = "blue")

abline(h=0.5, lty = 2)

legend("topright", legend = c("n = 5", "n = 15", "n = 50"), pch = 1, col = c("black", "red", "blue"))
text(0.5, 0.97, "!H0 more likely", adj = 0.5)
text(0.5, 0.03, "H0 more likely", adj = 0.5)


