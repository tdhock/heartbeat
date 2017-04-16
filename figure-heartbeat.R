works_with_R(
  "3.3.3",
  seewave="2.0.5", tuneR="1.3.2",
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

