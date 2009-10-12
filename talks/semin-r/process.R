system("python convert.py > HOCKING-latticedl-semin-r.Rnw");Sweave("HOCKING-latticedl-semin-r.Rnw",width=60)
##system(". ~/.bashrc;pdflatex HOCKING-latticedl-semin-r && evince HOCKING-latticedl-semin-r.pdf &")
## Remove plot()
system(". ~/.bashrc; python killplot.py HOCKING-latticedl-semin-r.tex && cp HOCKING-latticedl-semin-r.tex.new HOCKING-latticedl-semin-r.tex && pdflatex HOCKING-latticedl-semin-r && evince HOCKING-latticedl-semin-r.pdf &")
