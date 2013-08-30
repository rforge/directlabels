works_with_R("3.0.1",grConvert="0.1.0")
svgs <- Sys.glob("data/*.svg")
from <- svgs[1]
to <- sub("svg","ps", from)
convertPicture(from, to)
