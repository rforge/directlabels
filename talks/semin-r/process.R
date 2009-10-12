system("python convert.py > HOCKING-latticedl-semin-r.Rnw");Sweave("HOCKING-latticedl-semin-r.Rnw");system(". ~/.bashrc; python killplot.py HOCKING-latticedl-semin-r.tex && pdflatex HOCKING-latticedl-semin-r && evince HOCKING-latticedl-semin-r.pdf &")

