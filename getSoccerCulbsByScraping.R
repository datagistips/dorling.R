library(XML)
library(RCurl)
library(rgdal)
 
source("DORLING.R")
 
# GEOGLA DEPARTEMENTS
deps <- readOGR(".", "DEPARTEMENT.shp")
 
# LIST URLS
html = htmlParse("http://www.le-footballeur.com/clubs_football-liste-departement.php", asText=FALSE)
urls <- paste("http://www.le-footballeur.com/", xpathSApply(html, "//div[@align='left']//a", xmlAttrs), sep="")
 
# GET NUMBER OF SOCCER CLUBS
out <- vector(mode='list')
for (i in 1:length(urls)) {
  url <- urls[i]
  
  # DEPT
  dep <- sub("^.*dept=([0-9AB]{2,3}).*$", "\\1", url)
  print(dep)
  if (dep %in% c("2A","2B")) {url <- "http://www.le-footballeur.com/clubs_football-region.php?id_region=7"}
  
  url = "http://www.le-footballeur.com/clubs_football-departement.php?dept=21&PHPSESSID=esf1dmcakq6ogcoje2ilrofme2"
  
  # N CLUBS
  html = htmlParse(url, asText=F)
  doc <- xpathSApply(html, "//table//div[preceding-sibling::form]", xmlValue)
  nClubs <- as.numeric(sub("^\n([0-9]+) clubs trouvés.*$", "\\1", doc))
  nClubs <- ifelse(is.na(nClubs), 0, nClubs)
  
  out[[i]] <- data.frame(dep, nClubs)
  print(out[[i]])
}
 
df <- do.call("rbind", out)
 
## COMPUTE
deps$nClubs <- df$nClubs[match(deps$CODE_DEPT, df$dep)]
deps$nClubs[is.na(deps$nClubs)] <- 0
 
## DORLINGS
params <- dorlingParams(deps, value=deps$nClubs, nrescales=50, niter=50, tol=1000)
dorlings.pt <- SpatialPointsDataFrame(params$xy, data=data.frame(dept = deps$CODE_DEPT, lib_dept = deps$NOM_DEPT, nClubs = deps$nClubs, radius=params$radius/1000, row.names=row.names(params$xy)))
 
## EXPORT
writeOGR(dorlings.pt, ".", "clubsFoot", "ESRI Shapefile", overwrite=TRUE)
 
 
# SHIFTING LINES
 
coords <- coordinates(deps)
points(coordinates(deps), col="green")
points(params$xy, col="red")
for (i in 1:length(deps)) {
  lines(c(coords[i,1], params$xy[i,1]), c(coords[i,2], params$xy[i,2]))
}
