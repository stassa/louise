library(Hmisc)

source('louise_learning_curve_data.r', local=T)

# Line types and colours
# Two-value vectors because the first one was originally for Thelma
plot.type <- 'b'
lin.typs <- 1:2
pnt.typs <- 1:2
systems.cols <- c('red','blue')

# Plot title and labels
title <- paste(learner,'- learning curve for',target)
x.lab <- 'Sampling rate' 
y.lab <- metric

# Error bar line and point types
bar.type = 'o'
rand.bar.col = 'gray48'
red.bar.col = 'magenta'
bas.bar.col = 'darkgreen'

# Large axis, ticks, label, line and point sizes
# Better for papers. Tweak for other uses.
cex.axis <- 2.70
cex.lab <- 2.8
cex <- 2.5
lwd.ticks=3.0
lwd <- 3.0
# Increased errorbar sizes.
cap <- 0.025

results.size <- length(learner.eval.mean)
x.axis <- learner.sampling.rates

# Calculate standard errors.
learner.eval.se <- learner.eval.sd / sqrt(results.size)

# Calculate plot limits
y.lim.max <- max(learner.eval.mean+learner.eval.se, learner.eval.mean+learner.eval.se) 
y.lim.min <- min(learner.eval.mean-learner.eval.se, learner.eval.mean-learner.eval.se)
y.lim <- c(y.lim.min, y.lim.max)
x.lim <- c(1, results.size + 0.5)

p <- par()
par(mar=c(5.3,6.1,4.0,0.8), mgp=c(4,1,0) )

plot(x.axis, main=title, learner.eval.mean, ylim=y.lim, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, cex.main=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

errbar(x.axis, learner.eval.mean, yplus=learner.eval.mean+learner.eval.se, yminus=learner.eval.mean-learner.eval.se, col=0, pch=1, type=bar.type, errbar.col=bas.bar.col, add=T, cap=cap, lwd=lwd)

axis(1, at=x.axis, labels=x.axis, cex.axis=cex.axis, cex.lab=cex.lab, padj=0.5, lwd.ticks=lwd.ticks)

par(p)
