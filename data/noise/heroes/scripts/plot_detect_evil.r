library(Hmisc)

#============================== Data ==============================
#==================================================================

#metric <- 'fpr'

noise <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

#metric <- 'err'
#metric <- 'fpr'
metric <- 'fnr'

# Error
thelma.err.mean <- c(0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
thelma.err.sd <- c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

louise.err.mean <- c(0.03703703703703707,0.07870370370370369,0.1648148148148148,0.20833333333333334,0.24999999999999992,0.3194444444444444,0.38796296296296295,0.44259259259259265,0.513888888888889,0.574074074074074)
louise.err.sd <- c(0.035460263250234755,0.013275193313337373,0.03201556167874058,0.060677787812770057,0.022680460581325737,0.04806299846418962,0.022448267340701444,0.017890588575542505,0.03359805718684759,0.01447659185130685)

#FPR
louise.fpr.mean <- c(0.025925925925925925,0.09074074074074073,0.08703703703703702,0.11296296296296296,0.1259259259259259,0.20555555555555555,0.14814814814814817,0.22222222222222224,0.2555555555555556,0.2333333333333333)
louise.fpr.sd <- c(0.06994674308254466,0.05483066170667272,0.07409978977492655,0.08069674491289383,0.044341444434735734,0.07481624086975648,0.06234262307630913,0.06295085819250351,0.07600367393393277,0.06773267518195163)

thelma.fpr.mean <- c(0,0,0,0,0,0,0,0,0,0)
thelma.fpr.sd <- c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

#FNR
louise.fnr.mean <- c(0,0.1,0.20370370370370372,0.26481481481481484,0.39999999999999997,0.4870370370370371,0.5518518518518518,0.6796296296296296,0.7759259259259259,0.9166666666666666)
louise.fnr.sd <- c(0.0,0.0292152088054302,0.0261891400439462,0.0315359006776415,0.053244095439130425,0.05384895922855407,0.04684855792842045,0.04942128123764961,0.051238286344313905,0.048800581175437945)

thelma.fnr.mean <- c(0,1,1,1,1,1,1,1,1,1)
thelma.fnr.sd <- c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

if (metric == 'acc') {

	louise.eval.mean <- louise.acc.mean
	louise.eval.sd <- louise.acc.sd

	thelma.eval.mean <- thelma.acc.mean
	thelma.eval.sd <- thelma.acc.sd

} else if (metric == 'err') {

	louise.eval.mean <- louise.err.mean
	louise.eval.sd <- louise.err.sd

	thelma.eval.mean <- thelma.err.mean
	thelma.eval.sd <- thelma.err.sd

} else if (metric == 'fnr') {

	louise.eval.mean <- louise.fnr.mean
	louise.eval.sd <- louise.fnr.sd

	thelma.eval.mean <- thelma.fnr.mean
	thelma.eval.sd <- thelma.fnr.sd

} else if (metric == 'fpr') {

	louise.eval.mean <- louise.fpr.mean
	louise.eval.sd <- louise.fpr.sd

	thelma.eval.mean <- thelma.fpr.mean
	thelma.eval.sd <- thelma.fpr.sd
}



#============================ Plotting ============================
#==================================================================


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
x.lab <- 'Noise amount' 
#y.lab <- 'Accuracy'

if (metric == 'acc') {
	y.lab <- 'Accuracy'
} else if (metric == 'err') {
	y.lab <- 'Error'
} else if (metric == 'fnr') {
	y.lab <- 'False Negative Rate'
} else if (metric == 'fpr') {
	y.lab <- 'False Positive Rate'
}

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
cex.axis <- 2.40
cex.lab <- 2.8
cex <- 2.5
lwd.ticks=3.0
lwd <- 3.0
# Increased legend text size
leg.cex <- 3
# Increased errorbar sizes.
cap <- 0.025

# Last result index.
lim <- length(louise.eval.mean)

thelma.eval.mean <- thelma.eval.mean[1:lim]
thelma.eval.sd <- thelma.eval.sd[1:lim]
louise.eval.mean <- louise.eval.mean[1:lim]
louise.eval.sd <- louise.eval.sd[1:lim]

noise.seq <- length(thelma.eval.mean)
#x.axis <- 1:noise.seq
x.axis <- noise

# Calculate standard errors.
thelma.eval.se <- thelma.eval.mean / sqrt(noise.seq)
louise.eval.se <- louise.eval.mean / sqrt(noise.seq)

y.lim.max <- max(thelma.eval.mean+thelma.eval.se, louise.eval.mean+louise.eval.se) + 0.1 # Space for legend
y.lim.min <- min(thelma.eval.mean-thelma.eval.se, louise.eval.mean-louise.eval.se)
y.lim <- c(y.lim.min, y.lim.max)
x.lim <- c(1, noise.seq + 0.5)

p <- par()
par(mar=c(5.3,6.1,1.0,0.8), mgp=c(4,1,0) )

plot(x.axis, thelma.eval.mean, ylim=y.lim, type=plot.type, lty=lin.typs[1], pch=pnt.typs[1], col=systems.cols[1], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x.axis, louise.eval.mean, ylim=y.lim, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis, cex=cex, lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

errbar(x.axis, thelma.eval.mean, yplus=thelma.eval.mean+thelma.eval.se, yminus=thelma.eval.mean-thelma.eval.se, col=0, pch=1, type=bar.type, errbar.col=red.bar.col, add=T, cap=cap, lwd=lwd)
errbar(x.axis, louise.eval.mean, yplus=louise.eval.mean+louise.eval.se, yminus=louise.eval.mean-louise.eval.se, col=0, pch=1, type=bar.type, errbar.col=bas.bar.col, add=T, cap=cap, lwd=lwd)

axis(1, at=x.axis, labels=x.axis, cex.axis=cex.axis, cex.lab=cex.lab, padj=0.5, lwd.ticks=lwd.ticks)

legend('bottomright', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)

par(p)
