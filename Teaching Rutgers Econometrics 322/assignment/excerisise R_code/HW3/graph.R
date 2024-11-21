qnorm(0.975)
##########################################
## Can't reject 5%
##########################################
pdf("5Cant.pdf")
qvalue = qnorm(0.975)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Can\'t Reject Null at 5% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, -0.8 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(.025)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                 expression(Phi^-1~(.975))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()

##########################################
## Can reject 95%
##########################################
pdf("5Rn.pdf")

qvalue = qnorm(0.975)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 5% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, -3.5 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(.025)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(.975))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()

##########################################
## Can reject 95% positive
##########################################
pdf("5Rp.pdf")

qvalue = qnorm(0.975)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 5% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, 3.5 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(.025)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(.975))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()


##########################################
## Can't reject 1%
##########################################
pdf("1Cant.pdf")
pvalue = 0.995
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Can\'t Reject Null at 1% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, 1.2 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.005)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(0.995))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()

##########################################
## Can reject 1%
##########################################
pdf("1Rn.pdf")
pvalue = 0.995
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 1% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')
# add x-axis
axis(1, 
     at = c(-qvalue, -4 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.005)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(0.995))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()

##########################################
## Can reject 99% positive
##########################################
pdf("1Rp.pdf")
pvalue = 0.995
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 1% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')
# add x-axis
axis(1, 
     at = c(-qvalue, 4 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.005)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(0.995))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()



##########################################
## Can't reject 10%
##########################################
pdf("10Cant.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Can\'t Reject Null at 10% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, 0.6 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.05)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(0.95))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')

dev.off()

##########################################
## Can reject 10%
##########################################
pdf("10Rn.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 10% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, -3 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.05)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(0.95))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()

##########################################
## Can reject 90% positive
##########################################
pdf("10Rp.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 10% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, 3 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.05)),
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(Phi^-1~(0.95))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()



##########################################
## P-Value
##########################################
pdf("Rpvalue.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'P-Value',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue ,0, qvalue), 
     padj = 0.5,
     labels = c(
                expression(-frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) )),
                0,
                expression(frac(bar(X)[n,0]~-~mu[0,0],frac(sigma, sqrt(n)) ))
                ), 
     cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()


##########################################
## Confidence Interval 90
##########################################
pdf("CI90.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = '90% Confidence Interval ',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')
axis(1, 
     at = c(-qvalue, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.05)),
                expression(Phi^-1~(0.95))), cex.axis=0.9,tck =-0.02
     )

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
segments(-qvalue, 0, x1 = qvalue, 0,
         col = 'blue', lty = 'solid', lwd = 10)
mtext(text = expression(frac(bar(X)[n,0]~-~mu['ci90',0],frac(sigma, sqrt(n)) )) , side = 1, line = 0.6, outer = T, at = 0,
      adj = NA, padj = 0.5, cex = 0.9, col = 'blue')

dev.off()

##########################################
## Confidence Interval 95
##########################################
pdf("CI95.pdf")

pvalue = 0.975
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = '95% Confidence Interval ',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')
axis(1, 
     at = c(-qvalue, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.025)),
                expression(Phi^-1~(0.975))), cex.axis=0.9,tck =-0.02
)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
segments(-qvalue, 0, x1 = qvalue, 0,
         col = 'blue', lty = 'solid', lwd = 10)
mtext(text = expression(frac(bar(X)[n,0]~-~mu['ci95',0],frac(sigma, sqrt(n)) )) , side = 1, line = 0.6, outer = T, at = 0,
      adj = NA, padj = 0.5, cex = 0.9, col = 'blue')

dev.off()

##########################################
## Confidence Interval 99
##########################################
pdf("CI99.pdf")

pvalue = 0.995
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = '99% Confidence Interval ',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')
axis(1, 
     at = c(-qvalue, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.005)),
                expression(Phi^-1~(0.995))), cex.axis=0.9,tck =-0.02
)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
segments(-qvalue, 0, x1 = qvalue, 0,
         col = 'blue', lty = 'solid', lwd = 10)
mtext(text = expression(frac(bar(X)[n,0]~-~mu['ci99',0],frac(sigma, sqrt(n)) )) , side = 1, line = 0.6, outer = T, at = 0,
      adj = NA, padj = 0.5, cex = 0.9, col = 'blue')

dev.off()








##########################################
## Can't reject 1% Mean Difference
##########################################
pdf("MF1Cant.pdf")
pvalue = 0.995
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Can\'t Reject Null at 1% Level',
      yaxs = 'i',
      xlab = '',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, 0.3 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.005)),
                expression( frac(  (bar(X)[n,0]~-~bar(Y)[m,0] )~-~(mu[1]~-~mu[2]) , sqrt(frac(sigma[1]^2, n) +frac(sigma[2]^2, m) )  )   ),
                '',
                expression(Phi^-1~(0.995))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()

##########################################
## Can reject 10% mean difference
##########################################
pdf("MF10Rn.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 10% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, -3.5 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.05)),
                expression( frac(  (bar(X)[n,0]~-~bar(Y)[m,0] )~-~(mu[1]~-~mu[2]) , sqrt(frac(sigma[1]^2, n) +frac(sigma[2]^2, m) )  )   ),
                0,
                expression(Phi^-1~(0.95))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()
##########################################
## Can reject 10% mean difference
##########################################
pdf("MF10Rp.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Reject Null at 10% Level',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue, 3.5 ,0, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.05)),
                expression( frac(  (bar(X)[n,0]~-~bar(Y)[m,0] )~-~(mu[1]~-~mu[2]) , sqrt(frac(sigma[1]^2, n) +frac(sigma[2]^2, m) )  )   ),
                0,
                expression(Phi^-1~(0.95))), cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()


##########################################
## Confidence Interval 99 mean difference
##########################################
pdf("MFCI99.pdf")

pvalue = 0.995
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = '99% Confidence Interval ',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')
axis(1, 
     at = c(-qvalue, qvalue), 
     padj = 0.5,
     labels = c(expression(Phi^-1~(0.005)),
                expression(Phi^-1~(0.995))), cex.axis=0.9,tck =-0.02
)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
segments(-qvalue, 0, x1 = qvalue, 0,
         col = 'blue', lty = 'solid', lwd = 10)
mtext(text = expression(frac(  (bar(X)[n,0]~-~bar(Y)[m,0] )~-~(mu[1]~-~mu[2]) , sqrt(frac(sigma[1]^2, n) +frac(sigma[2]^2, m) )  )) , side = 1, line = 0.6, outer = T, at = 0,
      adj = NA, padj = 0.5, cex = 0.9, col = 'blue')

dev.off()


##########################################
## P-Value mean Difference
##########################################
pdf("MFRpvalue.pdf")

pvalue = 0.95
qvalue = qnorm(pvalue)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'P-Value for Mean Difference',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-qvalue ,0, qvalue), 
     padj = 0.5,
     labels = c(
       expression(-frac(  (bar(X)[n,0]~-~bar(Y)[m,0] )~-~(mu[1]~-~mu[2]) , sqrt(frac(sigma[1]^2, n) +frac(sigma[2]^2, m) )  )),
       0,
       expression( frac(  (bar(X)[n,0]~-~bar(Y)[m,0] )~-~(mu[1]~-~mu[2]) , sqrt(frac(sigma[1]^2, n) +frac(sigma[2]^2, m) )  ))
     ), 
     cex.axis=1)

# shade p-value/2 region in Middle
polygon(x = c(-qvalue, seq(-qvalue, qvalue, 0.01), qvalue),
        y = c(0, dnorm(seq(-qvalue, qvalue, 0.01)),0), 
        col = 'lightblue')
# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -qvalue, 0.01), -qvalue),
        y = c(0, dnorm(seq(-6, -qvalue, 0.01)),0), 
        col = 'lightcoral')

# shade p-value/2 region in right tail
polygon(x = c(qvalue, seq(qvalue, 6, 0.01), 6),
        y = c(0, dnorm(seq(qvalue, 6, 0.01)), 0), 
        col = 'lightcoral')
dev.off()










# NOT RUN {
# Simple example with a single polygon
x = c(0, 1.8, 1.8, 1, 1, 3, 3, 2.2, 2.2, 4,
      4, 6, 6, 14, 14, 6, 6,  4, 4, 0, 0)
y = c(0, 0, -2, -2, -10, -10, -2, -2, 0, 0,
      1.8, 1.8, 1, 1, 3, 3, 2.2, 2.2, 4, 4, 0)
xy = data.frame(x,y)
library(sp)
xy.sp = SpatialPolygons(list(Polygons(list(Polygon(xy)), ID = "test")))
plot(xy.sp, col = "khaki")
polygonsLabel(xy.sp, "Hi!")


# Example with multiple polygons, text labels and colours
x1 = c(0, 4, 4, 0, 0)
y1 = c(0, 0, 4, 4, 0)
x2 = c(1, 1, 3, 3, 1)
y2 = c(-2, -10, -10, -2, -2)
x3 = c(6, 14, 14, 6, 6)
y3 = c(1, 1, 3, 3, 1)
xy.sp = SpatialPolygons(list(
  Polygons(list(Polygon(cbind(x1,y1))), ID = "test1"), # box
  Polygons(list(Polygon(cbind(x3,y3))), ID = "test3"), # wide
  Polygons(list(Polygon(cbind(x2,y2))), ID = "test2")  # high
))
plot(xy.sp, col=terrain.colors(3))
labels=c("Hi!", "A very long text string", "N\na\nr\nr\no\nw")

# Note that the label for the tall and narrow box is
# not necessarily centred vertically in the box.
# The reason is that method="maxdist" minimises the 
# maximum distance from the label box to the surrounding
# polygon, and this distance is not changed by moving
# the label vertically, as long the vertical distance
# to the polygon boundary is less than the horizontal
# distance. For regular polygons like this, the other
# label positions (e.g., method="buffer") work better.
polygonsLabel(xy.sp, labels, cex=.8,
              col = c('white', 'black', 'maroon'))


# }
# NOT RUN {
## Example showing how bad the centroid 
## position can be on real maps.

# Needed libraries
if (require(maps) && require(maptools) && require(rgdal)) {
  
  # Load map data and convert to spatial object
  nmap = map("world", c("Norway", "Sweden", "Finland"),
             exact = TRUE, fill = TRUE, col = "transparent", plot = FALSE)
  nmap.pol = map2SpatialPolygons(nmap, IDs = nmap$names,
                                 proj4string = CRS("+init=epsg:4326"))
  nmap.pol = spTransform(nmap.pol, CRS("+init=epsg:3035"))
  
  # Plot map, centroid positions (red dots) and optimal
  # label positions using the <U+2018>buffer<U+2019> method.
  plot(nmap.pol, col = "khaki")
  nmap.centroids = polygonsLabel(nmap.pol, names(nmap.pol),
                                 method = "centroid", doPlot = FALSE)
  points(nmap.centroids, col = "red", pch=19)
  polygonsLabel(nmap.pol, names(nmap.pol), method = "buffer", cex=.8)
}
# }

# plot the standard normal density on the domain [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Rejection Region of a Right-Sided Test',
      yaxs = 'i',
      xlab = 't-statistic',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add the x-axis
axis(1, 
     at = c(-4, 0, 1.64, 4), 
     padj = 0.5,
     labels = c('', 0, expression(Phi^-1~(.95)==1.64), ''))

# shade the rejection region in the left tail
polygon(x = c(1.64, seq(1.64, 4, 0.01), 4),
        y = c(0, dnorm(seq(1.64, 4, 0.01)), 0), 
        col = 'darkred')
