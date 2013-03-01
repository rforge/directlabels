source("functions.R")
works_with_R("2.15.2",grImport="0.8.4")

ps.df <- read.csv("ps.csv")
ps.df$xml <- NA
owd <- setwd(tempdir())
for(i in 1:nrow(ps.df)){
  ps.base <- ps.df$ps[i]
  ps.file <- file.path(owd, ps.base)
  xml.base <- sub("ps$","xml", ps.base)
  xml.file <- file.path(owd, xml.base)
  cat(sprintf("%s -> %s\n",ps.base, xml.base))
  tryCatch({
    PostScriptTrace(ps.file,xml.file)
    ps.df$xml[i] <- xml.base
  },error=function(e){
    unlink(xml.file)
  })
}
setwd(owd)
write.csv(ps.df,"xml.csv",row.names=FALSE,quote=FALSE)
