library(Hmisc)

source('thelma_learning_rate_data.r', local=T)
source('louise_learning_rate_data.r', local=T)

# Line types and colours
plot.type <- 'b'
lin.typs <- 1:2
pnt.typs <- 1:2
systems.cols <- c('red','blue')

# Plot title and labels - mostly unused to make space for more plot
title <- 'Predictive accuracy comparison'
title.thelma <- 'Thelma accuracy'
title.louise <- 'Louise accuracy'
title.plain <- 'Predictive accuracy and sampling rate'
subtitle <- ''
x.lab <- 'Examples' 
y.lab <- 'Accuracy'

# Legend
leg.text <- c('Thelma', 'Louise')
leg.lin.cols <- systems.cols
leg.lin.typs <- lin.typs
leg.pnt.typs <- pnt.typs
leg.lwd <- 4.0

# Error bar line and point types
bar.type = 'o' # try 'b'
rand.bar.col = 'gray48'
red.bar.col = 'magenta'
bas.bar.col = 'darkgreen'

# Increased axis, ticks, label, line and point sizes
# Better for papers.
cex.axis <- 2.70
cex.lab <- 2.8
cex <- 2.5
lwd.ticks=3.0
lwd <- 3.0
# Increased legend text size
leg.cex <- 3
# Increased errorbar sizes.
cap <- 0.025

results.length <- length(thelma.eval.mean)
x.axis <- thelma.sampling.rates

# Calculate standard errors.
thelma.eval.se <- thelma.eval.sd / sqrt(results.length)
louise.eval.se <- louise.eval.sd / sqrt(results.length)

y.lim.max <- max(thelma.eval.mean+thelma.eval.se, louise.eval.mean+louise.eval.se) + 0.1 # Space for legend
y.lim.min <- min(thelma.eval.mean-thelma.eval.se, louise.eval.mean-louise.eval.se)
y.lim <- c(y.lim.min, y.lim.max)
x.lim <- c(1, results.length + 0.5)

p <- par()
par(mar=c(5.3,6.1,1.0,0.8), mgp=c(4,1,0) )

plot(x.axis, thelma.eval.mean, ylim=y.lim, type=plot.type, lty=lin.typs[1], pch=pnt.typs[1], col=systems.cols[1], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, louise.eval.mean, ylim=y.lim, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

errbar(x.axis, thelma.eval.mean, yplus=thelma.eval.mean+thelma.eval.se, yminus=thelma.eval.mean-thelma.eval.se, col=0, pch=1, type=bar.type, errbar.col=red.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, louise.eval.mean, yplus=louise.eval.mean+louise.eval.se, yminus=louise.eval.mean-louise.eval.se, col=0, pch=1, type=bar.type, errbar.col=bas.bar.col, add=T, cap=cap, lwd=lwd)

axis(1, at=x.axis, labels=x.axis, cex.axis=cex.axis, cex.lab=cex.lab, padj=0.5, lwd.ticks=lwd.ticks)

legend('topleft', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)

par(p)
