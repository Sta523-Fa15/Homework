suppressMessages(library(jsonlite))
suppressMessages(library(rgdal))
suppressMessages(library(rgeos))

args = commandArgs(trailingOnly = TRUE)

stopifnot(length(args) == 1)
team = args[1]

if (!file.exists("boroughs.json"))
{
    stop("No boroughs.json to score!")    
} 

pred = readOGR("boroughs.json", layer="OGRGeoJSON", verbose=FALSE)
load(file = "nybb.Rdata")


stopifnot("Name" %in% names(pred@data))

score = 0
for(b in bb@data$BoroName)
{
    pred_i = which(pred@data$Name == b)
    bb_i   = which(bb@data$BoroName == b)

    if (length(pred_i) == 0)
        stop("Borough of ",b," missing from predicted boundaries.")

    score = score + suppressWarnings(gArea(gSymdifference(pred[pred_i,], bb[bb_i,])))
}

score = score / suppressWarnings(gArea(bb))

writeLines(toJSON(list("score" = score), auto_unbox=TRUE), paste0(team,".json"))
