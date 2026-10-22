plot.pdbart <- function(x, xind = seq_len(length(x$fd)), plquants = c(0.05, 0.95), cols = c('black','blue'), type='b', ylab = "",  # my add
...) {
  rgy = range(x$fd)
  for(i in xind) {
    tsum = apply(x$fd[[i]],2,quantile,probs=c(plquants[1],.5,plquants[2]))
    plot(range(x$levs[[i]]),rgy,type='n',xlab=x$xlbs[i],ylab=ylab,...)
    lines(x$levs[[i]],tsum[2,],col=cols[1],type=type)  # I changed type='b' to type=type
    lines(x$levs[[i]],tsum[1,],col=cols[2],type=type)
    lines(x$levs[[i]],tsum[3,],col=cols[2],type=type)
  }
}
