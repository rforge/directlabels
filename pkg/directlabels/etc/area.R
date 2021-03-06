require(ggplot2)
require(plyr)
require(RColorBrewer)
require(RCurl)
require(directlabels)

link <- getURL("http://dl.dropbox.com/u/25609375/so_data/final.txt")
dat <- read.csv(textConnection(link), sep=' ', header=FALSE)


dat <- ddply(dat, .(episode), transform, percent = count / sum(count))

# needed to make geom_area not freak out because of missing value
dat2 <- rbind(dat, data.frame(count = 0, name = 'lane',
                             episode = '02-tea-leaves', percent = 0))

g <- ggplot(arrange(dat2,name,episode), aes(x=episode,y=percent)) + 
  geom_area(aes(fill=name, group = name), position='stack') +
  scale_fill_brewer()
print(g)
## TODO: I would think this should work:
direct.label(g,"last.qp")
## the dude wrote this:
g1 <- g + geom_dl(method='last.points', aes(label=name))
