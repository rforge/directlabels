index.html: table.R pngs.csv functions.R
	R --vanilla < $<
pngs.csv: plotflags.R xml.csv functions.R
	R --vanilla < $<
xml.csv: trace.R ps.csv functions.R
	R --vanilla < $<
ps.csv: svg2ps_csv.py svg2ps.py svg.csv
	python $< svg.csv ps.csv
svg.csv: download.R functions.R
	R --vanilla < $<
