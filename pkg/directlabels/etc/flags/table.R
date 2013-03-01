source("functions.R")
works_with_R("2.15.2",brew="1.0.6")

df <- read.table("pngs.csv",sep=",",header=TRUE)

to.display <- c(svg=".svg",svg.png="-svg.png",ps.png="-ps.png",
                R.png="-xml.png",R.pdf.png="-pdf.png")
out <- data.frame(state=rownames(df))
rownames(out) <- rownames(df)
for(colname in names(to.display)){
  out[,colname] <- NA
  for(state in rownames(df)){
    base <- sub(".svg$","",df[state,"svg"])
    img.file <- paste(base, to.display[colname], sep="")
    if(file.exists(img.file)){
      out[state, colname] <-
        sprintf('<a href="%s"><img src="%s" width="%s" /></a>',
                img.file, img.file, png.width, png.height)
    }
  }
}

brew("index-template.html","index.html")
##sortable.html.table(out, "index.html", page.title="Plotting SVGs to R")
