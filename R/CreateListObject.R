#' Generates a network list from a food web
#'
#' Gets a network in the base bipartite package format into a list format. N.B. Throughout this package
#'  uses hosts to refer to the focal layer, and 'wasps' the response layer, although this could equally
#'   be 'plants' and 'pollinators'.
#'
#' @param web in format specified by the bipartite package. Rows = focal layer, columns = response layer
#'
#' @return A network list for use with other functions in EcoLinkPredict package
#' @export
#'
#' @examples
#' data(Safariland, package = 'bipartite')
#' demolist<-CreateListObject(Safariland)
#' str(demolist)

CreateListObject<- function(web){


  DF<- bipartite::empty(web)


  SF <- list('obs'= DF,
             'n_hosts' = nrow(DF),
             'n_wasps' = ncol(DF),
             'WaspNames' = colnames(DF),
             'HostNames' = rownames(DF))

}
