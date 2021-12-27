library(BayesFactor)

par(mfrow = c(1,2))

getP <- function(n=20, effectSize = 0, sd = 1){
  x = rnorm(n, mean = 0, sd= sd) 
  y = rnorm(n, mean = effectSize, sd= sd)
  return(t.test(x,y)$p.value)
}


plot(NULL, xlim = c(0,1), ylim = c(0,3), xlab = "p-value", ylab = "probability of observing p", main = "Expected p distribution")

abline(h=1)

alt1 = replicate(10000, getP(n=10, effectSize = 0.5))
x = hist(alt1, breaks = 20, plot = F)
lines(x$mids, x$density, type = "both",pch = 1, col = "blue")

alt2 = replicate(10000, getP(n=50, effectSize = 0.5))
x = hist(alt2, breaks = 20, plot = F)
lines(x$mids, x$density, type = "both", pch = 2, col = "red")


legend("topright", legend = c("null", "n=10", "n=50"), pch = 1, col = c("black", "blue", "red"),cex=0.5)



getResult <- function(n=10, effectSize = 0.5, sd = 1,rscale="medium"){
  x = rnorm(n, mean = 0, sd= sd) 
  y = rnorm(n, mean = effectSize, sd= sd) 
  p = t.test(x,y)$p.value
  lmod = lm(x ~ 1)
  #lmod2 = lm(x ~ 0)
  #anova(lmod, lmod2)
  BF = extractBF(ttestBF(x,y,rscale = rscale))$bf
  prob = BF/(BF + 1)
  return(c(p = p, probAlt = prob))
}

sim1 = as.data.frame(t(replicate(500, getResult(n = 20))))
sim2 = as.data.frame(t(replicate(500, getResult(n = 50))))
sim3 = as.data.frame(t(replicate(500, getResult(n = 100))))

sim4 = as.data.frame(t(replicate(500, getResult(n = 20, rscale="ultrawide"))))
sim5 = as.data.frame(t(replicate(500, getResult(n = 50, rscale="ultrawide"))))
sim6 = as.data.frame(t(replicate(500, getResult(n = 100, rscale="ultrawide"))))

#sim4 = as.data.frame(t(replicate(500, getResult(n = 20, rscale=3))))
#sim5 = as.data.frame(t(replicate(500, getResult(n = 50, rscale=3))))
#sim6 = as.data.frame(t(replicate(500, getResult(n = 100, rscale=3))))

plot(probAlt ~ p, data = sim1, ylim = c(0,1), xlim = c(0,0.6), ylab = "Pr(H1|data)", xlab = "p-value", main = "BF vs. p-value")
points(probAlt ~ p, data = sim2, col = "red")
points(probAlt ~ p, data = sim3, col = "violet")

points(probAlt ~ p, data = sim4, col = "black",pch=15)
points(probAlt ~ p, data = sim5, col = "red",pch=15)
points(probAlt ~ p, data = sim6, col = "violet",pch=15)


abline(h=0.5, lty = 2)

legend("topright", legend = c("n = 20", "n = 50", "n = 100"), pch = 1, col = c("black", "red", "violet"),cex=0.5)
text(0.5, 0.97, "H1 more likely", adj = 0.5)
text(0.5, 0.03, "H0 more likely", adj = 0.5)

# Power of tests considered here
power.t.test(n = 10, delta = 0.5, type="two.sample")
power.t.test(n = 20, delta = 0.5, type="two.sample")
power.t.test(n = 50, delta = 0.5, type="two.sample")
power.t.test(n = 100, delta = 0.5, type="two.sample")

# Power of tests in the previous one-sample example
power.t.test(n = 2, delta = 0.5, type="one.sample")
power.t.test(n = 15, delta = 0.5, type="one.sample")
power.t.test(n = 20, delta = 0.5, type="one.sample")
power.t.test(n = 100, delta = 0.5, type="one.sample")

# Get back to the bayes factor
meanBF<-function(sim){mean(sim$probAlt/(1-sim$probAlt))}
meanBF(sim1)-meanBF(sim4) # diff with standard prior vs flatter prior
meanBF(sim2)-meanBF(sim5)
meanBF(sim3)-meanBF(sim6)
# for large sample sizes, the flatter the prior, the larger the Bayes factor
