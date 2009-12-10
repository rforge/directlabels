dldoc <- function # Make directlabels documentation
### Positioning Functions for direct labeling are supposed to work
### with only certain plot types. Each Positioning Function is defined
### in R/file.R and plot examples that it can be used in are found in
### tests/doc/file/*.R so that we can automatically assemble a
### database of example plots from the code.
(pkgdir=".."
### Package directory root.
 ){
  odir <- setwd(pkgdir)
  on.exit(setwd(odir))
  docdir <- file.path("tests","doc")
  docdirs <- dir(docdir)
  plotfiles <- sapply(docdirs,function(d)dir(file.path(docdir,d),full=TRUE))
  Rfiles <- paste(file.path("R",docdirs),".R",sep="")
  posfuns <- lapply(Rfiles,extract.posfun)
  names(posfuns) <- docdirs
  plots <- lapply(plotfiles,lapply,extract.plot)
  m <- cbind(plots,posfuns,type=names(plots))
  html <- apply(m,1,makehtml)
  m
}
extract.posfun <- function # Extract Positioning Function for documentation
(f
 ){
  L <- extract.docs.file(f)
  e <- new.env()
  sys.source(f,e)
  for(N in names(L)){
    L[[N]]$fun <- e[[N]]
    L[[N]]$name <- N
  }
  L
}
extract.plot <- function # Extract plot and definition for documentation
### Given an R code file, execute it, store the definition, and save
### the resulting plot in a variable.
(f
### 
 ){
  require(directlabels)
  code <- readLines(f)
  i <- max(grep("^\\w",code))
  code[i] <- paste("p <-",code[i])
  writeLines(code,tf <- tempfile())
  e <- new.env()
  sys.source(tf,e)
  list(code=paste(code,collapse="\n"),
       plot=e$p,
       title=sub(".R$","",basename(f)))
}
makehtml <- function # Make HTML docs
### Make plots and HTML for documentation website.
(L
### List of positioning functions and plots to match up.
 ){
  wwwdocs <- file.path("..","..","www","docs",L$type)
  dir.create(wwwdocs,recursive=TRUE)
  for(p in L$plots){
    cat(p$title,":",sep="")
    for(f in L$posfuns){
      cat(" ",f$name,sep="")
      png(file.path(wwwdocs,paste(p$title,f$name,"png",sep=".")))
      print(direct.label(p$plot,f$fun))
      dev.off()
    }
    cat("\n")
  }
}
