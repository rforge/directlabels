## contour plot
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")
v <- ggplot(volcano3d, aes(x, y, z = z))
v + stat_contour(aes(colour = ..level..))
