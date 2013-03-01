source("functions.R")
works_with_R("2.15.2",SortableHTMLTables="0.1.3")

df <- read.table("pngs.csv",sep=",",header=TRUE)
for(from.suffix in c("ps","svg","pdf")){
  files <- df[,from.suffix]
  for(from.file in files){
    to.file <- sub("[.].*$",sprintf("-%s.png",from.suffix),from.file)
    cmd <- sprintf("convert %s -geometry %dx%d %s",
                   from.file, png.width, png.height, to.file)
    cat(cmd,"\n")
    system(cmd)
  }
}

to.display <- c(svg=".svg",svg.png="-svg.png",ps.png="-ps.png",
                R.png="-xml.png",R.pdf.png="-pdf.png")
out <- data.frame(state=rownames(df))
rownames(out) <- rownames(df)
for(colname in names(to.display)){
  out[,colname] <- NA
  for(state in rownames(df)){
    base <- sub(".svg$","",df[state,"svg"])
    out[state, colname] <-
      sprintf('<img src="%s%s" width="%s",height="%s" />',
              base, to.display[colname], png.width, png.height)
  }
}
              
sortable.html.table(out, "index.html", page.title="Plotting SVGs to R")
