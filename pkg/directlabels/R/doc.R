dldoc <- function # Make direct label documentation
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
  Rfiles <- paste(file.path("R",docdirs),".R",sep="")
  lapply(Rfiles,extract.pfs)
}
extract.pfs <- function # Extract Positioning Functions
### Given an R source file, we extract a list of PFs with (1) function
### name (2) function definition as text (3) value in R (4)
### description as annotated in source.
(Rfile
 ){
  ## basically we should just use inlinedocs for all of this.
### List of Positioning Functions and descriptions.
}
