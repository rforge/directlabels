library(grImport)
xml.files <- Sys.glob(file.path("data","*.xml"))
pics <- list()
for(x in xml.files){
  p <- tryCatch(readPicture(x),error=function(e)NULL)
  if(!is.null(p))pics <- c(pics,list(p))
}
pdf("Rflags.pdf")
for(p in pics){
  grid.newpage()
  grid.picture(p)
}
dev.off()
