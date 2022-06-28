library(data.table)
library(igraph)
library(tibble)
library(dplyr)
library(stringr)
library(pbmcapply)

edges <- fread('data/network/openstreetmapGraph.csv')
edges <- edges %>%
  dplyr::filter(YEAR == '2011')

graph <- igraph::graph.data.frame(edges, directed=TRUE)
edgesID <- unique(edges$ID)

walkingList <- pbmclapply(edgesID, function(id){
  print(id)
  if(id == 'ID235'){
    break
  } else{
    bfsList <- bfs(graph = graph, root = V(graph)['ID3767'], neimode = 'out', order = TRUE, dist = TRUE, unreachable = FALSE)
    distanceDf <- data.frame('distance' = bfsList$dist)
    distanceDf <- rownames_to_column(distanceDf, var = 'node')
    distanceDf <- distanceDf %>%
      dplyr::filter(distance <= 6)
    distanceDf[distanceDf==""]<-NA
    distanceDf <- distanceDf %>%
      na.omit()
    allPath <- shortest_paths(graph, from = V(graph)['ID3767'], to = V(graph)[distanceDf$node],
                              mode = 'out', output = 'vpath')
    walkStrings <- pbmclapply(allPath$vpath, function(x){
      pathName <- x$name
      predicate <- edge_attr(graph, name = 'description', index = E(graph, path=x))
      predicate <- append(predicate, '')
      walkString <- paste(pathName, predicate, collapse = ' ')
      return(walkString)
    }, mc.cores=cores)
    return(walkStrings)
  }
}, mc.cores = cores)


testList <- bfs(graph = graph, root = V(graph)['ID3767'], neimode = 'out', order = TRUE, dist = TRUE, unreachable = FALSE)
distanceDf <- data.frame('distance' = testList$dist)
distanceDf <- rownames_to_column(distanceDf, var = 'node')
distanceDf <- distanceDf %>%
  dplyr::filter(distance <= 6)
distanceDf[distanceDf==""]<-NA
distanceDf <- distanceDf %>%
  na.omit()
allPath <- shortest_paths(graph, from = V(graph)['ID3767'], to = V(graph)[distanceDf$node], mode = 'out')
test <- allPath$vpath[2][[1]]$name
testWalkStrings <-pbmclapply(allPath$vpath, function(x){
  pathName <- x$name
  predicate <- edge_attr(graph, name = 'description', index = E(graph, path=x))
  predicate <- c(predicate, '')
  walkString <- paste(pathName, predicate, collapse = ' ')
  return(walkString)
}, mc.cores = cores)
