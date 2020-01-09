louise <- c(1.0000,1.0000,0.9050,0.7640,0.6460,0.5160,0.4100,0.3620,0.2920,0.2350,0.2070)
thelma <- c(1.0000,1.0000,0.9170,0.7680,0.6460,0.5290,0.4300,0.3390,0.2900,0.2680,0.2220)

# Line types and colours
plot.type <- 'b'
lin.typs <- 1:2
pnt.typs <- 1:2
systems.cols <- c('red','blue')

# Plot title and labels
title <- 'Robot planning performance comparison'
title.smth <- paste(title, '(smoothed)')
title.log <- paste(title, '(log scale)')
title.thelma <- 'Thelma accuracy'
title.louise <- 'Louise accuracy'
title.plain <- 'Accuracy vs grid world size'
subtitle <- ''
x.lab <- 'Grid dimensions (side)' 
y.lab <- 'Accuracy'

# Legend
#leg.text <- c('Primitives selected by relevance', 'Baseline (no selection)')
leg.text <- c('Thelma', 'Louise')
leg.lin.cols <- systems.cols
leg.lin.typs <- lin.typs
leg.pnt.typs <- pnt.typs

# Increased axis, ticks, label, line and point sizes
# Better for papers.
cex.axis <- 2.0
cex.lab <- 2.0
cex <- 2.5
lwd.ticks=3.0
lwd = 3.0
# Increased legend text size
leg.cex <- 1.8


len <- length(louise)
x <- 1:len

p <- par()
par(mar=c(5.2,5.5,1.0,0.8), mgp=c(3.8,1,0) )

plot(x, thelma, type=plot.type, lty=lin.typs[1], pch=pnt.typs[1], col=systems.cols[1], xlab=x.lab, ylab=y.lab, xaxt='n', cex.axis=cex.axis,cex=cex,lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)
lines(x, louise, type=plot.type, lty=lin.typs[2], pch=pnt.typs[2], col=systems.cols[2], xlab='', ylab='', xaxt='n', cex.axis=cex.axis,cex=cex,lwd=lwd, cex.lab=cex.lab, lwd.ticks=lwd.ticks)

axis(1,at=1:len,labels=1:len, cex.axis=cex.axis,cex.lab=cex.lab,padj=1.0,lwd.ticks=lwd.ticks)

legend('topright',inset=0.02,legend=leg.text,lty=leg.lin.typs,pch=leg.pnt.typs,col=leg.lin.cols,cex=leg.cex,lwd=lwd)

par(p)
