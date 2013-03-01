source("functions.R")
u <- url("http://en.wikipedia.org/wiki/Flags_of_the_U.S._states")
html.lines <- readLines(u)
close(u)
grep("svg",html.lines,value=TRUE)
html <- paste(html.lines,collapse="\n")
pattern <- paste("title=\"Flag of ",
                 "(?<state>.+?)",
                 " *(?:[([].*?[])])? *[(]",
                 "(?<datestr>[A-Z][a-z]+ [0-9]+, [0-9]{4})",sep="")
m <- str_match_all_perl(html,pattern)[[1]]
states <- data.frame(m[1:50,-1])
stopifnot(length(unique(states$state))==nrow(states))
states <- within(states,{
  date <- strptime(datestr,"%B %d, %Y")
})
states[order(states$date),]
dir.create("data",FALSE)

## cache state web pages to a local directory.
state.pages <- c()
for(state in states$state){
  p <- gsub(" ","_",state)
  cat(state,p,"\n")
  f <- file.path("data",sprintf("%s.html",p))
  state.pages[state] <- if(file.exists(f)){
    paste(readLines(f),collapse="\n")
  }else{
    u <- url(sprintf("http://en.wikipedia.org/wiki/File:Flag_of_%s.svg",p))
    html <- paste(readLines(u),collapse="\n")
    close(u)
    cat(html,file=f)
    html
  }
}

svg.pattern <- paste("(?<dompath>upload.*?Flag_of_",
                     "(?<basename>.*?[.]svg)",
                     ")",
                     sep="")
svg.info <- str_match_perl(state.pages, svg.pattern)
svg.urls <- sprintf("http://%s",svg.info[,"dompath"])
svg.df <- data.frame()
for(i in seq_along(svg.urls)){
  destfile <- file.path("data",svg.info[i,"basename"])
  svg.df <- rbind(svg.df,data.frame(filename=destfile))
  u <- svg.urls[i]
  if(!file.exists(destfile)){
    cat(sprintf("%s -> %s\n",u,destfile))
    download.file(u,destfile)
  }
}

write.csv(svg.df,"svg.csv",row.names=FALSE,quote=FALSE)
