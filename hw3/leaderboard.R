suppressMessages(library(knitr))
suppressMessages(library(jsonlite))
suppressMessages(library(stringr))

files = dir(pattern = "Team.*\\.json")

d = data.frame(
    teams = str_replace(files,"\\.json",""),
    scores = unlist(lapply(files, fromJSON))
)

s = order(d$scores,decreasing=FALSE)

print( kable(d[s,],row.names = FALSE) )
