## This function (define_quadrants()) splits a square around each state
## into four even sections. This is done so that each download is smaller
## so it can be downloaded at higher resolution

## Returns coordinats for bounding boxes for four quadrants in each state

define_quadrants <- function(xmin, xmax, ymin, ymax){
  xhalf <- (xmax - xmin) / 2
  yhalf <- (ymax - ymin) / 2
  
  xmin1 <- xmin
  xmax1 <- xmin1 + xhalf
  ymin1 <- ymin + yhalf
  ymax1 <- ymax
  
  xmin2 <- xmax1 - 0.0001
  xmax2 <- xmax
  ymin2 <- ymin1
  ymax2 <- ymax1
  
  xmin3 <- xmin1
  xmax3 <- xmax1
  ymin3 <- ymin
  ymax3 <- ymin1 -0.0001
  
  xmin4 <- xmin2
  xmax4 <- xmax2
  ymin4 <- ymin3
  ymax4 <- ymax3
  
  return(list(c(xmin1, xmax1, ymin1, ymax1),
              c(xmin2, xmax2, ymin2, ymax2),
              c(xmin3, xmax3, ymin3, ymax3),
              c(xmin4, xmax4, ymin4, ymax4)))
}