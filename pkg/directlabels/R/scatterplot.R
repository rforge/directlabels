### Calculate closest point on the alpha hull with size of the boxes,
### and put it outside that point.
closest.on.ahull <- function(d,...){
  edges.to.outside(ahull.points(d),visualcenter(d),...)
}

### Calculate closest point on the convex hull and put it outside that
### point. Assume d is the center for each point cloud and then use
### orig.data to calculate hull.
closest.on.chull <- function(d,...){
  edges.to.outside(chull.points(d),visualcenter(d),...)
}
### Use bounding box information with a small empty.grid to find the a
### non-colliding label that is close to a point on the convex hull,
### which is close to the visual center of the data. 
smart.grid <- list("big.boxes","empty.grid")

### Use empty.grid with perpendicular.lines.
perpendicular.grid <- list("perpendicular.lines","empty.grid")

### Use empty.grid with extreme.points.
extreme.grid <- list("extreme.points","big.boxes","empty.grid")

