library(Hmisc)

#thelma.acc.mean <- c(0.06666666666666667,0.10714285714285712,0.11538461538461539,0.2333333333333333,0.20909090909090908,0.26999999999999996,0.3555555555555556,0.35,0.4428571428571428,0.4666666666666667)
#thelma.acc.sd <- c(0.0,0.050507627227610534,0.0747557935236577,0.06573421981221796,0.10540925533894598,0.13374935098492585,0.10210406483029794,0.16457014715109583,0.21769770382214212,0.21942686286812776)
#
#louise.acc.mean <- c(0.06666666666666668,0.24285714285714283,0.38461538461538464,0.45000000000000007,0.7,1.0,1.0,1.0,1.0,1)
#louise.acc.sd <- c(0.0769800358919501,0.09035079029052512,0.21452821193181423,0.3244178071256985,0.30318178032195786,0.0,0.0,0.0,0.0,0.0)

louise.acc.mean <- c(0.04375,0.125,0.48026315789473684,0.9087837837837838)
louise.acc.sd <- c(0.023935677693908454,0.05273060974039773,0.2704046077508483,0.18243243243243246)

thelma.acc.mean <- c(0.04375,0.125,0.48026315789473684,0.9087837837837838)
thelma.acc.sd <- c(0.023935677693908454,0.05273060974039773,0.2704046077508483,0.18243243243243246)


# Line types and colours
plot.type <- 'b'
lin.typs <- 1:2
pnt.typs <- 1:2
systems.cols <- c('red','blue')

# Plot title and labels
title <- 'Predictive accuracy comparison'
title.smth <- 'Predictive accuracy comparison (smoothed)'
title.log <- 'Predictive accuracy comparison (log scale)'
title.thelma <- 'Thelma accuracy'
title.louise <- 'Louise accuracy'
title.plain <- 'Predictive accuracy and hypothesis size'
subtitle <- ''
#x.lab <- expression(paste('Size of ',Theta[k]))
x.lab <- 'Examples' 
y.lab <- 'Accuracy'

# Legend
#leg.text <- c('Primitives selected by relevance', 'Baseline (no selection)')
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

# Last result index.
lim <- length(louise.acc.mean)

thelma.acc.mean <- thelma.acc.mean[1:lim]
thelma.acc.sd <- thelma.acc.sd[1:lim]
louise.acc.mean <- louise.acc.mean[1:lim]
louise.acc.sd <- louise.acc.sd[1:lim]

th.size <- length(thelma.acc.mean)
x.axis <- 1:th.size

# Calculate standard errors.
thelma.acc.se <- thelma.acc.mean / sqrt(th.size)
louise.acc.se <- louise.acc.mean / sqrt(th.size)

y.lim.max <- max(thelma.acc.mean+thelma.acc.se, louise.acc.mean+louise.acc.se) + 0.1 # Space for legend
y.lim.min <- min(thelma.acc.mean-thelma.acc.se, louise.acc.mean-louise.acc.se)
y.lim <- c(y.lim.min, y.lim.max)
x.lim <- c(1, th.size + 0.5)

p <- par()
par(mar=c(5.3,6.1,1.0,0.8), mgp=c(4,1,0) )

plot(x.axis, thelma.acc.mean, ylim=y.lim, type=plot.type, lty=lin.typs[1], pch=pnt.typs[1], col=systems.cols[1], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, louise.acc.mean, ylim=y.lim, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

errbar(x.axis, thelma.acc.mean, yplus=thelma.acc.mean+thelma.acc.se, yminus=thelma.acc.mean-thelma.acc.se, col=0, pch=1, type=bar.type, errbar.col=red.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, louise.acc.mean, yplus=louise.acc.mean+louise.acc.se, yminus=louise.acc.mean-louise.acc.se, col=0, pch=1, type=bar.type, errbar.col=bas.bar.col, add=T, cap=cap, lwd=lwd)

axis(1, at=x.axis, labels=x.axis, cex.axis=cex.axis, cex.lab=cex.lab, padj=0.5, lwd.ticks=lwd.ticks)

legend('topleft', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)

par(p)
