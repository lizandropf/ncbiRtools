read_PubMedRec <- function(PMIDfile, rettype){
  recFile <- list()
  pubmedF <- readLines(PMIDfile, n=-1L, warn=FALSE)
  if (rettype=="docsum"){
    gLins <- grep("^[0-9]*: ", pubmedF)
    for (i in 1:length(gLins)){
      ifelse (gLins[i]==gLins[length(gLins)], m <-  length(pubmedF), m <- gLins[i + 1])
      recFile[[i]] <- list()
      recFile[[i]]$FullRecord <- sub(" *[0-9]*: ", "", paste(pubmedF[gLins[[i]]:(m-1)], collapse=" "), "[.] ")
      recFile[[i]]$PMID <- sub("[.]", "", rev(grep("[0-9]*[.]", strsplit(sub(" *[0-9]*: ", "", paste(pubmedF[gLins[[i]]:(m-1)], collapse=" "), "[.] "), " ")[[1]],value = TRUE))[[1]])
      if (length(grep("doi:", recFile[[i]]$FullRecord))==0){
        recFile[[i]]$doi <- "No DOI"
      }
      else{
        recFile[[i]]$doi <- sub("[.] .*", "", sub(".*doi: ", "", recFile[[i]]$FullRecord))
      }
    }
  }
  else if (rettype=="abstract"){
    gLins <- grep("^[0-9]*[.] ", pubmedF)
    for (i in 1:length(gLins)){
      recFile[[i]] <- list()
      if (i==length(gLins)){
        m <- length(pubmedF)
        recFile[[i]]$FullRecord <- paste(pubmedF[gLins[[i]]:m], sep="\n", collapse="\n")
        recFile[[i]]$doi <- sub(" ", "", sub("^DOI: ", "", grep("DOI: ", pubmedF[gLins[[i]]:m], value = TRUE)))
        recFile[[i]]$PMID <- sub(" ", "", sub(" *\\[.*\\]", "", sub("^PMID: ", "", grep("PMID: ", pubmedF[gLins[[i]]:m], value = TRUE))))
      }
      else if (i<length(gLins)){
        m <- i + 1
        recFile[[i]]$FullRecord <- paste(pubmedF[gLins[[i]]:(gLins[[m]]-1)], sep="\n", collapse="\n")
        recFile[[i]]$doi <- sub(" ", "", sub("^DOI: ", "", grep("DOI: ", pubmedF[gLins[[i]]:(gLins[[m]]-1)], value = TRUE)))
        recFile[[i]]$PMID <- sub(" ", "", sub(" *\\[.*\\]", "", sub("^PMID: ", "", grep("PMID: ", pubmedF[gLins[[i]]:(gLins[[m]]-1)], value = TRUE))))
      }
    }
  }
  recFile <- unlist(recFile, recursive=FALSE)
  recFile
}
