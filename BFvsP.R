library(BayesFactor)

getP <- function(n=10, effectSize = 0, sd = 1){
  x = rnorm(n, mean = effectSize, sd= sd) 
  return(t.test(x)$p.value)
}

pdf(file = "Fig1_1sampleTtest.pdf",width=10,height=5)

par(mfrow = c(1,2))

plot(NULL, xlim = c(0,1), ylim = c(0,6), xlab = "p-value", ylab = "frequency of obs. of p", main = "p distribution")

abline(h=1)

alt1 = replicate(10000, getP(n=15, effectSize = 0.2))
x = hist(alt1, breaks = 20, plot = F)
lines(x$mids, x$density, type = "both", pch = 1, col = "blue")

alt2 = replicate(10000, getP(n=100, effectSize = 0.2))
x = hist(alt2, breaks = 20, plot = F)
lines(x$mids, x$density, type = "both", pch = 0, col = "red")

alt3 = replicate(10000, getP(n=200, effectSize = 0.2))
x = hist(alt3, breaks = 20, plot = F)
lines(x$mids, x$density, type = "both", pch = 2, col = "orange")

legend("topright", legend = c("null", "n = 15, power = 10%", "n = 100, power = 50%","n = 200, power = 80%"), lty = c(1,NA,NA,NA), pch = c(NA,1,0,2), col = c("black", "blue", "red","orange"))



getResult <- function(n=10, effectSize = 0.2, sd = 1){
  x = rnorm(n, mean = effectSize, sd= sd) 
  p = t.test(x)$p.value
  lmod = lm(x ~ 1)
  #lmod2 = lm(x ~ 0)
  #anova(lmod, lmod2)
  BF = extractBF(ttestBF(x))$bf
  odds = BF/(BF + 1)
  return(c(p = p, probAlt = odds))
}

sim1 = as.data.frame(t(replicate(500, getResult(n = 10))))
sim2 = as.data.frame(t(replicate(500, getResult(n = 20))))
sim3 = as.data.frame(t(replicate(500, getResult(n = 100))))


plot(probAlt ~ p, data = sim1, ylim = c(0,1), xlim = c(0,0.6), ylab = "Pr(H1|data)", xlab = "p-value", main = "BF vs. p-value")
points(probAlt ~ p, data = sim2, col = "blue")
points(probAlt ~ p, data = sim3, col = "red")

abline(h=0.5, lty = 2)

legend("top", legend = c("n = 10", "n = 20", "n = 100"), pch = 1, col = c("black", "blue", "red"))
text(0.5, 0.97, "H1 more likely", adj = 0.5)
text(0.5, 0.03, "H0 more likely", adj = 0.5)

dev.off()

# Power of tests in the one-sample example
power.t.test(n = 10, delta = 0.2, type="one.sample")
power.t.test(n = 15, delta = 0.2, type="one.sample")
power.t.test(n = 20, delta = 0.2, type="one.sample")
power.t.test(n = 100, delta = 0.2, type="one.sample")
power.t.test(n = 200, delta = 0.2, type="one.sample")

