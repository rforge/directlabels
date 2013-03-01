source("functions.R")
works_with_R("2.15.2",grImport="0.8.4")

xml.df <- read.csv("xml.csv")
xml.df$png <- NA
xml.df$pdf <- NA
rownames(xml.df) <- gsub("_"," ",gsub(".ps$","",gsub(".*/","",xml.df$ps)))

read.or.null <- function(x){
  tryCatch(readPicture(x),error=function(e)NULL)
}

for(i in 1:nrow(xml.df)){
  xml.file <- as.character(xml.df$xml[i])
  png.file <- sub("[.]xml$","-xml.png",xml.file)
  pdf.file <- sub("[.]xml$",".pdf",xml.file)
  p <- read.or.null(xml.file)
  if(!is.null(p)){
    png(png.file, png.height, png.width)
    grid.picture(p)
    dev.off()
    xml.df$png[i] <- png.file

    pdf(pdf.file)
    grid.picture(p)
    dev.off()
    xml.df$pdf[i] <- pdf.file
  }
}

for(from.suffix in c("ps","svg","pdf")){
  files <- xml.df[,from.suffix]
  for(from.file in files){
    to.file <- sub("[.].*$",sprintf("-%s.png",from.suffix),from.file)
    cmd <- sprintf("convert %s -geometry %dx%d %s",
                   from.file, png.width, png.height, to.file)
    cat(cmd,"\n")
    system(cmd)
  }
}

write.table(xml.df,"pngs.csv",quote=FALSE,row.names=TRUE,sep=",")
