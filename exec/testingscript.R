#testing stuff

#lines
plotPV(data[1:4,"leaf"=5], x='fresh.weight', y='water.potential')

x='fresh.weight'; y='water.potential'

d<-data[data$leaf==5,]

rows=4

slope<-pvest::sma_slope(d[[x]][1:rows], d[[y]][1:rows])
int<-pvest::sma_intercept(d[[x]][1:rows], d[[y]][1:rows], slope)

l.x = seq(min(d[[y]]), max(d[[y]]),length.out=length(d[[x]]))
l.y = int-(slope*l.x)

ggplot(data=d, aes(x=fresh.weight, y=water.potential))+
  geom_point(color='black', size=3)+
  geom_line(aes(x=l.x, y=l.y))

plot(d[[x]]~d[[y]], 
     ylab=y, 
     xlab=x)

lines(x=l.x, y=l.y, lwd=8)


