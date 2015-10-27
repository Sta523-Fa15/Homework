library(sp)
library(stringr)
library(rgeos)
library(jsonlite)

write_geojson = function(sp, file)
{
  stopifnot(class(sp) == "SpatialPolygonsDataFrame")
  
  sp = createSPComment(sp)
  
  poly_json = function(x)
  {
    owners = as.integer(str_split(comment(x)," ")[[1]])
    
    paste("[",
          paste(
            sapply(which(owners == 0), function(i)
            {
              res = "[ ["
              
              res = paste(res, paste("[", apply(x@Polygons[[i]]@coords, 1, paste, collapse=", "), "]", collapse=", "))
              
              for (j in which(i %in% owners))
              { 
                res = paste(res, "], [")
                
                res = paste(res, paste("[", apply(x@Polygons[[j]]@coords, 1, paste, collapse=", "), "]", collapse=", "))
              }
              
              res = paste(res, "] ]")
            }),
            collapse = ", "
          ),
          "]")
  }
  qt = function(x) paste0('"',x,'"')
  
  res = paste('{',
              '"type": "FeatureCollection",',
              '"features": [',
              paste(
                sapply(1:nrow(sp), function(i)
                {
                  paste('{ "type": "Feature",',
                        '"properties": { ',
                        paste(qt(names(sp)), qt(sp@data[i,]), sep=": ", collapse=", "),
                        ' },',
                        '"geometry": {',
                        '    "type": "MultiPolygon",',
                        '    "coordinates": ',
                        poly_json(sp@polygons[[i]]),
                        '} }',
                        sep="\n")
                }),
                collapse=",\n"
              ),
              '] }')

  write(prettify(res), file = file)
}