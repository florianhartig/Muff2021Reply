library(BayesFactor)

getP <- function(n=10, effectSize = 0, sd = 1){
  x = rnorm(n, mean = effectSize, sd= sd) 
  return(t.test(x)$p.value)
}

pdf(file = "Fig1_1sampleTtest.pdf",width=9,height=5)

par(mfrow = c(1,2))

plot(NULL, xlim = c(0,1), ylim = c(0,6), xlab = "p-value", 
     ylab = "frequency", main = "Simulated p-distribution")

abline(h=1)

reps = 500000

alt1 = replicate(reps, getP(n=13, effectSize = 0.2))
x = hist(alt1, breaks = 15, plot = F)
lines(x$mids, x$density, type = "both", pch = 1, col = "blue")

alt2 = replicate(reps, getP(n=200, effectSize = 0.2))
x = hist(alt3, breaks = 15, plot = F)
lines(x$mids, x$density, type = "both", pch = 2, col = "orange")

legend("topright", 
       legend = c("H0 (no effect)", "H1, n = 13, power = ~10%", "H1, n = 200, power = ~80%"), 
       lty = c(1,NA,NA), 
       pch = c(NA,1,2), 
       col = c("black", "blue", "orange"), 
       bty = "n")



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

sim1 = as.data.frame(t(replicate(500, getResult(n = 13))))
sim2 = as.data.frame(t(replicate(500, getResult(n = 200))))


plot(probAlt ~ p, data = sim1, ylim = c(0,1), xlim = c(0,0.6), ylab = "Pr(H1|data)", xlab = "p-value", main = "BF vs. p-value", col = "blue")
points(probAlt ~ p, data = sim2, col = "orange", pch = 2)

abline(h=0.5, lty = 2)

legend("topright", 
       legend = c("n = 13", "n = 200"), 
       pch = c(1,2), 
       col = c("blue", "orange"), 
       bty = "n")
text(0.6, 0.55, "H1 more likely", adj = 1)
text(0.6, 0.45, "H0 more likely", adj = 1)


mtext('text is here', side=1, line=3.5, at=9)
mtext('text is here', side=1, line=3.5, at=9)

dev.off()

# Power of tests in the one-sample example
power.t.test(n = 10, delta = 0.2, type="one.sample")
power.t.test(n = 15, delta = 0.2, type="one.sample")
power.t.test(n = 20, delta = 0.2, type="one.sample")
power.t.test(n = 100, delta = 0.2, type="one.sample")
power.t.test(n = 200, delta = 0.2, type="one.sample")

