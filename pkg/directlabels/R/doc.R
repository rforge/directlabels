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
  plotfiles <- sapply(docdirs,function(d)Sys.glob(file.path(docdir,d,"*.R")))
  Rfiles <- paste(file.path("R",docdirs),".R",sep="")
  posfuns <- lapply(Rfiles,extract.posfun)
  names(posfuns) <- docdirs
  plots <- lapply(plotfiles,lapply,extract.plot)

  ## add crosslinks between positioning functions
  repfuns <- apply(cbind(lapply(posfuns,names),names(posfuns)),1,function(L){
    REP <- paste('<a href="../../',L[[2]],
                 '/posfuns/\\1.html">\\1</a>',sep='')
    function(def,ignore){
      items <- L[[1]][L[[1]]!=ignore]
      if(length(items)){
        FIND <- paste("(",paste(items,collapse="|"),")",sep="")
        gsub(FIND,REP,def)
      }else def
    }
  })
  repall <- function(def,ignore){
    for(f in repfuns)def <- f(def,ignore)
    def
  }
  posfuns <- lapply(posfuns,function(L)lapply(L,function(LL){
    LL$definition <- repall(LL$definition,LL$name)
    LL
  }))

  ## matrix of all extracted data to process
  m <- cbind(plots,posfuns,type=names(plots))

  setwd(file.path("..","..","www","docs"))
  foot <- paste(readLines("templates/foot.html"),collapse="\n")
  makehtml <- function # Make HTML documentation
  ## Make plots and HTML for documentation website.
  (L
   ## List of positioning functions and plots to match up.
   ){
    ## all paths are relative to the docs directory
    subdir <- L$type
    pngurls <- matrix("",nrow=length(L$posfuns),ncol=length(L$plots),
                      dimnames=list(names(L$posfuns),
                        sapply(L$plots,function(x)x$name)))
    ## first make plots
    datanames <- names(L)[sapply(L,class)=="list"]
    tomake <- file.path(subdir,c("",datanames))
    for(d in tomake)
      if(!file.exists(d))dir.create(d,recursive=TRUE)
    for(p in L$plots){
      cat(p$name,":",sep="")
      for(f in L$posfuns){
        pngfile <- file.path(subdir,paste(p$name,f$name,"png",sep="."))
        pngurls[f$name,p$name] <- pngfile
        if(!file.exists(pngfile)){
          cat(" ",f$name,sep="")
          png(pngfile)
          print(direct.label(p$plot,f$fun))
          dev.off()
        }
        thumbfile <- file.path(subdir,paste(p$name,f$name,"thumb.png",sep="."))
        if(!file.exists(thumbfile)){
          cmd <- paste("convert -geometry 64x64",pngfile,thumbfile)
          cat(" ",thumbfile,sep="")
          system(cmd)
        }
      }
      cat("\n")
    }
    ## now make html for plot examples
    makepage <- function(item,items,row,main){
      if(length(items)){
        tmp <- lapply(items,function(f){
          pngurl <- if("fun"%in%names(f))pngurls[f$name,item$name]
          else pngurls[item$name,f$name]
          c(f,pngurl=file.path("..","..",pngurl),
            parname=item$name,
            url=file.path("..",row,paste(f$name,".html",sep="")))
        })
        rowfile <- paste("templates/",row,"-row.html",sep="")
        rowhtml <- sapply(tmp,filltemplate,rowfile)
        item$table <- paste(c("<table>",rowhtml,"</table>"),collapse="\n")
      }
      item$type <- L$type
      item$pagetitle <- item$name
      item$head <- filltemplate(item,"templates/head.html")
      item$foot <- foot
      html <- filltemplate(item,paste("templates/",main,".html",sep=""))
      write(html,file.path(subdir,main,paste(item$name,".html",sep="")))
      item
    }
    res <- list()
    for(i in seq_along(datanames)){
      this <- datanames[i]
      that <- datanames[-i]
      res[[this]] <- lapply(L[[this]],makepage,L[[that]],that,this)
    }
    res
  }
  res <- apply(m,1,makehtml)
  extract.links <- function(L){
    sapply(names(L),function(N)if(N=="type")L[[N]] else {
      ##if(L$type=="utility.function")browser()
      x <- sapply(L[[N]],function(x)x$name)
      content <- if(N=="plots"){
        ann <- paste(x,L$posfuns[[1]]$name,sep=".")
        paste('<img alt="',ann,'" src="',L$type,
              "/",ann,".thumb.png",'" />',sep="")
      } else x
      if(length(x))
        paste(paste("<a href=\"",L$type,"/",N,"/",x,".html\">",content,"</a>",
                    sep=""),collapse="\n<br />\n")
      else x
    },simplify=FALSE)
  }
  links <- apply(m,1,extract.links)
  tmp <- list(head=filltemplate(list(pagetitle="home"),"templates/head.html"),
              foot=foot)
  rows <- lapply(links,filltemplate,"templates/index-row.html")
  tmp$table <- paste(rows,collapse="\n")
  html <- filltemplate(tmp,"templates/index.html")
  write(html,"index.html")

  m
### Matrix of lists describing example plots and matching builtin
### Positioning Functions.
}
extract.posfun <- function # Extract Positioning Function for documentation
### Use inlinedocs to extract comments and definitions from code, then
### for each item found add the value and its name to the list.
(f
### R code file, which should contain only Positioning Functions that
### can be used with examples defined in the doc/ subdirectory with
### the same name.
 ){
  require(inlinedocs)
  require(directlabels)
  L <- extract.docs.file(f)
  e <- new.env()
  sys.source(f,e)
  for(N in names(L)){
    L[[N]]$fun <- e[[N]]
    L[[N]]$name <- N
    L[[N]]$definition <- rhtmlescape(L[[N]]$definition)
  }
  L
### List of lists, each of which describes one Positioning Function
### defined in f.
}
extract.plot <- function # Extract plot and definition for documentation
### Given an R code file, execute it, store the definition, and save
### the resulting plot in a variable.
(f
### R code file with plot example.
 ){
  require(directlabels)
  code <- readLines(f)
  i <- max(grep("^\\w",code))
  code[i] <- paste("p <-",code[i])
  writeLines(code,tf <- tempfile())
  e <- new.env()
  sys.source(tf,e)
  code <- rhtmlescape(code)
  list(code=paste(code,collapse="\n"),
       plot=e$p,
       name=sub(".R$","",basename(f)))
}
rhtmlescape <- function
### for standards compliance we should escape <>&
(code
### R code to be displayed on a HTML page between pre tags.
 ){
  code <- gsub("[&]","&amp;",code)
  code <- gsub("[<]","&lt;",code)
  code <- gsub("[>]","&gt;",code)
### Standards compliant HTML to display.
}

filltemplate <- function
### Fill in occurances of OBJ$item in the file template with the value
### in R of L$item.
(L,template){
  txt <- paste(readLines(template),collapse="\n")
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
  txt
}
