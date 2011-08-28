library(ggplot2)
tx <- time(mdeaths)
Time <- ISOdate(floor(tx),round(tx%%1 * 12)+1,1,0,0,0)
uk.lung <- rbind(data.frame(Time,sex="male",deaths=as.integer(mdeaths)),
                 data.frame(Time,sex="female",deaths=as.integer(fdeaths)))
qplot(Time,deaths,data=uk.lung,colour=sex,geom="line")
