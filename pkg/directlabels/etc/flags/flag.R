library(grImport)
for(ps.file in Sys.glob("ps/*.ps")){
  print(ps.file)
  xml.file <- sub("ps$","xml",ps.file)
  PostScriptTrace(ps.file,xml.file)
}
pics <- lapply(Sys.glob("xml/*.xml"),readPicture)
for(p in pics){
  grid.newpage()
  grid.picture(p)
  Sys.sleep(2)
}
## why do I get error 1 from gs with this file?
PostScriptTrace("ps/Kansas.ps","xml/Kansas.xml")
## how about this?
