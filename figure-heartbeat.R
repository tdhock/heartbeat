works_with_R(
  "3.3.3",
  seewave="2.0.5", tuneR="1.3.2",
  Segmentor3IsBack="2.0",
  "tdhock/coseg@49d056963d8c0eaebea22fef453842255a330a05",
  ggplot2="2.1.0")

heartbeat <- readMP3("heartbeat.mp3")
first7sec <- heartbeat[1:300000]
first.env <- env(first7sec)
total.seconds <- duration(first7sec)
first.sec <- seq(0, total.seconds, l=length(first.env))

## downsample for quick analysis...
some.sec <- seq(0, total.seconds, l=4000)
some.env <- approx(first.sec, first.env, some.sec)$y
plot(some.sec, some.env, type="l")

## because the peak model is only Poisson loss implemented for the
## Poisson loss...
some.int <- as.integer(some.env)
plot(some.sec, some.int, type="l")
range(some.int)
some.rle <- rle(some.int)

sec.diff <- diff(some.sec[1:2])
count.dt <- with(some.rle, data.table(
  count=values,
  lengths))
count.dt[, chromStart := as.integer(c(0, cumsum(lengths[-.N])))]
count.dt[, chromEnd := as.integer(cumsum(lengths))]
count.dt[, seconds := chromEnd*sec.diff]
max.peaks <- 17L
fit <- PeakSegPDPAchrom(count.dt, max.peaks)
peaks <- data.table(fit$segments)[peaks==max.peaks & status=="peak"]
setkey(count.dt, chromEnd)
start <- count.dt[peaks$chromStart, seconds]
end <- count.dt[peaks$chromEnd, seconds]
peaks[, start.seconds := start]
peaks[, end.seconds := end]

max.segments <- max.peaks*2+1
fit.segmentor <- Segmentor(some.int, Kmax=max.segments)

seg.mean.vec <- fit.segmentor@parameters[max.segments,]
seg.end.vec <- fit.segmentor@breaks[max.segments,]
seg.start.vec <- c(1L, seg.end.vec[-max.segments]+1)
data.mean.vec <- rep(seg.mean.vec, seg.end.vec-seg.start.vec+1)
segmentor.seg.dt <- data.table(
  start=seg.start.vec,
  end=seg.end.vec,
  mean=seg.mean.vec,
  start.seconds=(seg.start.vec-1)*sec.diff,
  end.seconds=seg.end.vec*sec.diff)
segmentor.break.dt <- data.table(
  pos=seg.start.vec[-1],
  seconds=segmentor.seg.dt$start.seconds[-1],
  diff=diff(seg.mean.vec))
segmentor.break.dt[, sign := sign(diff)]
segmentor.break.dt[, feasible.sign := c(1, -1)]
segmentor.break.dt[, status := ifelse(
  sign==feasible.sign, "feasible", "infeasible")]

gg <- ggplot()+
  theme_bw()+
  geom_line(aes(
    seconds, count),
    data=count.dt)+
  geom_segment(aes(
    start.seconds, mean,
    xend=end.seconds, yend=mean),
    data=segmentor.seg.dt,
    color="green",
    size=2)+
  geom_vline(aes(
    linetype=status,
    xintercept=seconds),
    data=segmentor.break.dt,
    color="green")+
  ylab("Amplitude (higher for louder heartbeat)")
print(gg)

segmentor.loss.dt <- data.table(
  model="unconstrained",
  total.PoissonLoss=PoissonLoss(some.int, data.mean.vec))
segmentor.loss.dt[, mean.PoissonLoss := total.PoissonLoss/length(data.mean.vec)]
constrained.loss.dt <- data.table(
  model="PeakSeg",
  total.PoissonLoss=subset(fit$loss, peaks==max.peaks)$PoissonLoss)
constrained.loss.dt[, mean.PoissonLoss := total.PoissonLoss/length(data.mean.vec)]
rbind(segmentor.loss.dt, constrained.loss.dt)

gg.unconstrained <- ggplot()+
  theme_bw()+
  geom_vline(aes(
    color=model,
    linetype=status,
    xintercept=seconds),
    data=data.table(segmentor.break.dt, model="unconstrained"))+
  geom_line(aes(
    seconds, count),
    data=count.dt)+
  geom_segment(aes(
    start.seconds, mean,
    color=model,
    xend=end.seconds, yend=mean),
    data=data.table(segmentor.seg.dt, model="unconstrained"),
    size=2)+
  geom_label(aes(
    6, 18000, label=sprintf("unconstrained PoissonLoss=%.0f", mean.PoissonLoss),
    color=model),
    hjust=1,
    data=segmentor.loss.dt)+
  ylab("Amplitude (higher for louder heartbeat)")+
  scale_color_manual(values=c(
    PeakSeg="#E41A1C",
    "deepskyblue",
    unconstrained="#377EB8",
    "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", 
    "#A65628", "#F781BF", "#999999"))+
  guides(color="none")
print(gg.unconstrained)
pdf("figure-heartbeat-unconstrained.pdf", 10)
print(gg.unconstrained)
dev.off()

constrained.segs <- data.table(fit$segments)[segments==max.segments]
constrained.segs[, start.seconds := chromStart*sec.diff]
constrained.segs[, end.seconds := chromEnd*sec.diff]
constrained.break.dt <- constrained.segs[, list(
  model="PeakSeg",
  pos=chromStart[-1],
  seconds=start.seconds[-1],
  diff=diff(mean))]
constrained.break.dt[, sign := sign(diff)]
constrained.break.dt[, feasible.sign := c(1, -1)]
constrained.break.dt[, status := ifelse(
  sign==feasible.sign, "feasible", "infeasible")]
gg.PeakSeg <- gg.unconstrained+
  geom_segment(aes(
    seconds, 15000,
    color=model,
    linetype=status,
    xend=seconds, yend=0),
    data=constrained.break.dt)+
  geom_segment(aes(
    start.seconds, mean,
    color=model,
    xend=end.seconds, yend=mean),
    data=data.table(constrained.segs, model="PeakSeg"),
    size=1)+
  geom_label(aes(
    6, 20000, label=sprintf("PeakSeg PoissonLoss=%.0f", mean.PoissonLoss),
    color=model),
    hjust=1,
    data=constrained.loss.dt)
pdf("figure-heartbeat-PeakSeg.pdf", 10)
print(gg.PeakSeg)
dev.off()

bpm <- data.table(
  beats=max.peaks,
  total.seconds,
  beats.per.minute=max.peaks/total.seconds * 60)
peak.stats <- peaks[, data.table(
  max.peaks,
  mean.duration.seconds=mean(end.seconds-start.seconds)
  )]
gg <- ggplot()+
  theme_bw()+
  ## bpm[, ggtitle(paste(
  ##   "Petit être, jeudi 13 avril 2017,",
  ##   beats, "battements dans", round(total.seconds, 2),
  ##   "secondes * 60 sec/min =", beats.per.minute,
  ##   "battements par minute"))]+
  bpm[, ggtitle(paste(
    "Little being, Thurs 13 April 2017,",
    beats, "beats in", round(total.seconds, 2),
    "seconds * 60 sec/min =", beats.per.minute,
    "beats per minute"))]+
  geom_line(aes(
    seconds, count),
    data=count.dt)+
  geom_segment(aes(
    start.seconds, mean,
    xend=end.seconds, yend=mean),
    data=peaks,
    color="red",
    size=2)+
  geom_text(aes(
    2, 22000, label=paste(
      "Most likely model with", max.peaks, "peaks,",
      "mean peak duration =", round(mean.duration.seconds, 2),
      "seconds")),
    color="red",
    data=peak.stats)+
  ylab("Amplitude (higher for louder heartbeat)")
pdf("figure-heartbeat.pdf", 10)
print(gg)    
dev.off()

