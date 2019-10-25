library(Hmisc)

#============================== Data ==============================
#==================================================================

#metric <- 'fpr'

noise <- c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

#metric <- 'err'
metric <- 'fpr'
#metric <- 'fnr'

# Error

# Msiclassified by flipping labels and balanced
thelma.err.mean <- c(0,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
thelma.err.sd <- c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

louise.err.mean <- c(0,0.05681818181818182,0.10454545454545454,0.16136363636363632,0.20227272727272724,0.23409090909090907,0.3022727272727273,0.34090909090909094,0.41136363636363643,0.4431818181818182)
louise.err.sd <- c(0.0,0.03258456540546451,0.038924947208076134,0.04475471359921796,0.059120039439777804,0.028446944478058463,0.05969966152983498,0.02395664894066952,0.04072630319913823,0.03258456540546452)

#FPR
thelma.fpr.mean <- c(0,0,0,0,0,0,0,0,0,0)
thelma.fpr.sd <- c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

louise.fpr.mean <- c(0.01818181818181818,0,0.013636363636363636,0,0.00909090909090909,0,0.004545454545454545,0,0.004545454545454545,0)
louise.fpr.sd <- c(0.04391326286724072,0.0,0.04312196809320516,0.0,0.02874797872880345,0.0,0.014373989364401724,0.0,0.014373989364401724,0.0)

#FNR
thelma.fnr.mean <- c(0,1,1,1,1,1,1,1,1,1)
thelma.fnr.sd <- c(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)

louise.fnr.mean <- c(0,0.10909090909090909,0.21818181818181817,0.22727272727272724,0.3818181818181819,0.55,0.6363636363636364,0.7181818181818181,0.7727272727272726,0.8999999999999998)
louise.fnr.sd <- c(0.0,0.0317820863081864,0.09534625892455922,0.07422696190252055,0.05335399048987579,0.1015264579863694,0.12309149097933272,0.07041787902195305,0.08834775598250459,0.051604201108867875)

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

legend('topright', inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)
# FNR with noise as sign flips
#legend(0.46,0.62, inset=0.02, legend=leg.text, lty=leg.lin.typs, pch=leg.pnt.typs, col=leg.lin.cols, cex=leg.cex, lwd=leg.lwd)

par(p)
