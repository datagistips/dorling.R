library(rgeos)
library(spdep)
 
 
# DISTANCE
 
distance <- function(pt1, pt2) {
  dx <- pt1[1] - pt2[1]
  dy <- pt1[2] - pt2[2]
  l <- sqrt(dx^2 + dy^2)  
  return(l)
}
 
 
# SCALING
 
dorlingRadius <- function(f, values, ratio) {
  
nb <- poly2nb(f)
 
cumdist <- 0
cumradius <- 0
 
for (i in 1:length(nb)) {
  
  if (!is.null(nb[[i]])) {
    
    neighs <- nb[[i]][which(nb[[i]] < i)]
    
    for (j in neighs) {
      
      l <- distance(coordinates(f)[i, ], coordinates(f)[j, ])
      d <- sqrt(values[i]/pi) + sqrt(values[j]/pi)
      cumdist <- cumdist + l
      cumradius <- cumradius + d
    }
  }
}
scale <- cumdist/cumradius
radiuses <- sqrt(values/pi) * scale * ratio
return(radiuses)
}
 
  
## SHIFTING
 
dorlingParams <- function(f, value, nrescales, niter, tol) {
 
sqs <- seq(1, 0, length.out=nrescales)
 
for (ratio in sqs) {
  coords <- coordinates(f)
  row.names(coords) <- row.names(f)
  radius <- dorlingRadius(f, value, ratio)
  
  for (iter in 1:niter) {
    
    overlapCount <- 0
    
    for (i in 1:nrow(coords)) {
      
      for (j in 1:nrow(coords)) {
        
        if (j != i) {
          
          dx <- coords[i, 1] - coords[j, 1]
          dy <- coords[i, 2] - coords[j, 2]
          l <- sqrt(dx^2 + dy^2)
          d <- radius[i] + radius[j]
          prop <- (l-d)/l
          dx <- dx * prop; #print(dx)
          dy <- dy * prop; #print(dy)
          
          if (l < d) {
            
            if (abs(l - d) > tol) {
              
              overlapCount <- overlapCount + 1
              coords[i, 1] <- coords[i, 1] - dx
              coords[i, 2] <- coords[i, 2] - dy  
            }
          }
        }
      }
    }
  }
  print(paste("scale factor of", ratio, "=>", "number of overlaps :", overlapCount))
  if (overlapCount == 0) break
}
return(list(xy=coords, radius=radius))
}
