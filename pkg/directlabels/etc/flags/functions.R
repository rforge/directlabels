options(repos=c(#"http://cran.miroir-francais.fr/",
          "http://www.bioconductor.org/packages/release/bioc",
          "http://r-forge.r-project.org",
          "http://cran.ism.ac.jp"))
png.height <- png.width <- 200
works_with_R <- function(Rvers,...){
  pkg_ok_have <- function(pkg,ok,have){
    stopifnot(is.character(ok))
    if(!as.character(have) %in% ok){
      warning("works with ",pkg," version ",
              paste(ok,collapse=" or "),
              ", have ",have)
    }
  }
  pkg_ok_have("R",Rvers,getRversion())
  pkg.vers <- list(...)
  for(pkg in names(pkg.vers)){
    if(!suppressWarnings(require(pkg, character.only=TRUE))){
      install.packages(pkg)
    }
    pkg_ok_have(pkg, pkg.vers[[pkg]], packageVersion(pkg))
    library(pkg, character.only=TRUE)
  }
}
str_match_perl <- function(string,pattern){
  parsed <- regexpr(pattern,string,perl=TRUE)
  captured.text <- substr(string,parsed,parsed+attr(parsed,"match.length")-1)
  captured.text[captured.text==""] <- NA
  captured.groups <- do.call(rbind,lapply(seq_along(string),function(i){
    if(is.na(parsed[i]) || parsed[i]==-1)return(NA)
    st <- attr(parsed,"capture.start")[i,]
    substring(string[i],st,st+attr(parsed,"capture.length")[i,]-1)
  }))
  result <- cbind(captured.text,captured.groups)
  colnames(result) <- c("",attr(parsed,"capture.names"))
  result
}
str_match_all_perl<- function(string,pattern){
  parsed <- gregexpr(pattern,string,perl=TRUE)
  lapply(seq_along(parsed),function(i){
    r <- parsed[[i]]
    starts <- attr(r,"capture.start")
    if(r[1]==-1)return(matrix(nrow=0,ncol=1+ncol(starts)))
    names <- attr(r,"capture.names")
    lengths <- attr(r,"capture.length")
    full <- substring(string[i],r,r+attr(r,"match.length")-1)
    subs <- substring(string[i],starts,starts+lengths-1)
    m <- matrix(c(full,subs),ncol=length(names)+1)
    colnames(m) <- c("",names)
    m
  })
}
