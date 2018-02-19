# Seach sequences for taxon
# WARNING, depending on search terms record ids can be of different taxa as the one searched for
searchGB <- function(term, db="nuccore", retmax=20){
  # plain text to search terms
  term= gsub(" ", "+", term, fixed = TRUE)
  # search strings
  S_address <- paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?', 'db=', db, '&term=', term, '&retmax=', retmax, sep='')
  # curl search strings
  R_rawtext <- curl(S_address)
  # read found ids
  R_rawIds <- readLines(R_rawtext, n=-1L, warn=FALSE)
  # close connection
  close(R_rawtext)
  # found counts, retmax, retstart
  F_count <- grep('><Count>[A-Z,0-9]', R_rawIds, value=TRUE)
  F_counts <- as.numeric(unlist(strsplit(F_count, "[a-zA-Z<>/ ]"))[unlist(strsplit(F_count, "[a-zA-Z<>/ ]"))!=""])
  F_ids <- list()
  F_ids$Counts <- F_counts[[1]]
  F_ids$Retmax <- F_counts[[2]]
  F_ids$Retstart <- F_counts[[3]]
  # found ids strings
  if (F_counts[[1]]>0){
    F_idsF <- grep('^<Id>[A-Z,0-9]', R_rawIds, value=TRUE)
    F_ids$Ids <- as.numeric(unlist(strsplit(F_idsF, "[<Id> </id>]"))[unlist(strsplit(F_idsF, "[<Id> </id>]"))!=""])
    cat(F_ids$Retmax, "out of", F_ids$Counts, "results recovered")
  }
  else{
    F_ids$Ids <- "No sequence found"
  }
  attributes(F_ids) <- list(class="SearGB")
  names(F_ids) <- c("Counts", "Retmax", "Retstart", "Ids")
  F_ids
}
