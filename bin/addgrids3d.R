#' Add grids to a scatterplot3d
#' 
#' @description The goal of this function is to add grids on an existing
#'  plot created using the package scatterplot3d
#' @param x,y,z numeric vectors specifying the x, y, z coordinates of points.
#'  x can be a matrix or a data frame containing 3 columns corresponding to
#'  the x, y and z coordinates. In this case the arguments y and z are optional
#' @param grid specifies the facet(s) of the plot on which grids should be drawn.
#'  Possible values are the combination of "xy", "xz" or "yz".
#'  Example: grid = c("xy", "yz"). The default value is TRUE to add grids only on xy facet.
#' @param col.grid,lty.grid color and line type to be used for grids
#' @param lab a numerical vector of the form c(x, y, len).
#'  The values of x and y give the (approximate) number of tickmarks on the x and y axes.
#' @param lab.z the same as lab, but for z axis
#' @param scale.y of y axis related to x- and z axis
#' @param angle angle between x and y axis
#' @param "xlim, ylim, zlim" the x, y and z limits (min, max) of the plot.
#' 
#' @note
#' Users who want to extend an existing scatterplot3d graphic with the
#'  function addgrids3d, should consider to set the arguments scale.y, angle, ...,
#'  to the value used in scatterplot3d.
#' 
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' 
#' @example
#' library(scatterplot3d)
#' data(iris)
#' scatterplot3d(iris[, 1:3], pch = 16, grid=T, box=F)
#' addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))
addgrids3d <- function(x, y=NULL, z=NULL, grid = TRUE,
                    col.grid = "grey", lty.grid = par("lty"),
                    lab = par("lab"), lab.z = mean(lab[1:2]),
                    scale.y = 1, angle = 40,
                    xlim=NULL, ylim=NULL, zlim=NULL){
  
  
  if(inherits(x, c("matrix", "data.frame"))){
    x <- as.data.frame(x)
    y <- unlist(x[,2])
    z <- unlist(x[,3])
    x <- unlist(x[,1])
  }
  
  p.lab <- par("lab")
  
  angle <- (angle%%360)/90
  yz.f <- scale.y * abs(if (angle < 1) angle else if (angle >3) angle - 4 else 2 - angle)
  yx.f <- scale.y * (if (angle < 2) 1 - angle else angle - 3)
  
  
  # x axis range
  x.range <- range(x[is.finite(x)], xlim)
  x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 *lab[1], p.lab[1])))
  x.scal <- round(diff(x.prty[1:2]), digits = 12)
  x <- x/x.scal
  x.range <- range(x.prty)/x.scal
  x.max <- ceiling(x.range[2])
  x.min <- floor(x.range[1])
  if (!is.null(xlim)) {
    x.max <- max(x.max, ceiling(xlim[2]/x.scal))
    x.min <- min(x.min, floor(xlim[1]/x.scal))
  }
  x.range <- range(x.min, x.max)
  
  # y axis range
  y.range <- range(y[is.finite(y)], ylim)
  y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 *lab[2], p.lab[2])))
  y.scal <- round(diff(y.prty[1:2]), digits = 12)
  y.add <- min(y.prty)
  y <- (y - y.add)/y.scal
  y.max <- (max(y.prty) - y.add)/y.scal
  if (!is.null(ylim))
    y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
  
  # Z axis range
  z.range <- range(z[is.finite(z)], zlim)
  z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 *lab.z, p.lab[2])))
  z.scal <- round(diff(z.prty[1:2]), digits = 12)
  z <- z/z.scal
  z.range <- range(z.prty)/z.scal
  z.max <- ceiling(z.range[2])
  z.min <- floor(z.range[1])
  if (!is.null(zlim)) {
    z.max <- max(z.max, ceiling(zlim[2]/z.scal))
    z.min <- min(z.min, floor(zlim[1]/z.scal))
  }
  z.range <- range(z.min, z.max)
  
  # Add grid
  if ("xy" %in% grid || grid == TRUE) {
    i <- x.min:x.max
    segments(i, z.min, i + (yx.f * y.max), yz.f * y.max + 
               z.min, col = col.grid, lty = lty.grid)
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min, x.max + 
               (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
  }
   
  if ("xz" %in% grid) {
    i <- x.min:x.max
    segments(i + (yx.f * y.max), yz.f * y.max + z.min, 
             i + (yx.f * y.max), yz.f * y.max + z.max, 
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.max + temp,temp1 + i , col = col.grid, lty = lty.grid)
    
  }
  
  if ("yz" %in% grid) {
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min,  
             x.min + (i * yx.f) ,i * yz.f + z.max,  
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.min, i , col = col.grid, lty = lty.grid)
    }
  
}