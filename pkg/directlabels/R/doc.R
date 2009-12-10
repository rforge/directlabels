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
  ## all paths are relative to the docs directory
  odir <- setwd(file.path("..","..","www","docs"))
  on.exit(setwd(odir))
  subdir <- L$type
  pngurls <- matrix("",nrow=length(L$posfuns),ncol=length(L$plots),
                 dimnames=list(names(L$posfuns),
                   sapply(L$plots,function(x)x$title)))
  ## first make plots
  tomake <- file.path(subdir,c("","plots","methods"))
  for(d in tomake)
    if(!file.exists(d))dir.create(d,recursive=TRUE)
  for(p in L$plots){
    cat(p$title,":",sep="")
    for(f in L$posfuns){
      cat(" ",f$name,sep="")
      pngfile <- file.path(subdir,paste(p$title,f$name,"png",sep="."))
      pngurls[f$name,p$title] <- pngfile
      ##png(pngfile)
      ##print(direct.label(p$plot,f$fun))
      ##dev.off()
    }
    cat("\n")
  }
  foot <- paste(readLines("templates/foot.html"),collapse="\n")
  ## now make html for plot examples
  for(p in L$plots){
    tmp <- lapply(L$posfuns,function(f)
                  c(f,url=file.path("..","..",pngurls[f$name,p$title])))
    rowhtml <- sapply(tmp,filltemplate,"templates/posfun-row.html")
    p$table <- paste(rowhtml,collapse="\n")
    p$type <- L$type
    p$pagetitle <- p$title
    p$head <- filltemplate(p,"templates/head.html")
    p$foot <- foot
    html <- filltemplate(p,"templates/plot.html")
    write(html,file.path(subdir,"plots",paste(p$title,".html",sep="")))
  }
}
filltemplate <- function
### Fill in occurances of OBJ$item in the file template with the value
### in R of L$item.
(L,template){
  txt <- paste(readLines(template),collapse="")
  L <- L[sapply(L,class)=="character"]
  locs <- gregexpr("OBJ[$]([a-z]+)\\b",txt)[[1]]
  keywords <- sapply(seq_along(locs),function(i)
                     substr(txt,locs[i]+4,locs[i]+
                            attr(locs,"match.length")[i]-1))
  FIND <- sapply(keywords,function(x)paste("OBJ[$]",x,sep=""))
  REP <- unlist(ifelse(keywords%in%names(L),L[keywords],""))
  for(i in seq_along(FIND)){
    txt <- gsub(FIND[i],REP[i],txt)
  }
  cat(txt,'\n\n')
  txt
}
