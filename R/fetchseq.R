# function to fetch sequences from ids retrieved in searchGB or NCBI accession numbers
fetchseq <- function(ids, db="nuccore", rettype="fasta", retmode="text"){
  a_time <- Sys.time()
  if (class(ids)=="SearGB"){
    ids <- ids$Ids
  }
  else if (!is.vector(ids)){
    cat("Function only supports ids from SearchGB or vectors containing NCBI ids or accession numbers")
  }
  id_s <- list()
  rettype <- tolower(rettype)
  pb <- txtProgressBar(min=0, max=length(ids), style=3)
  for (i in 1:length(ids)){
    id <- as.character(ids[[i]])
    searchid <- paste('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?', 'db=', db, '&id=', id, '&rettype=', rettype, '&retmode=', retmode, sep='')
    c.file <- curl(searchid)
    if(rettype=="fasta"){
      id_s[[i]] <- read_fasta(c.file)
    }
    else if(rettype=="gb"){
      id_s[[i]] <- read_gb(c.file)
    }
    else{
      cat("Please select either fasta or gb as rettype, no support for other rettypes")
    }
    close(c.file)
    b_time <- Sys.time()
    if (b_time - a_time < 1 && i%%4==0){
      Sys.sleep(1)
      a_time <- Sys.time()
    }
    setTxtProgressBar(pb, i)
  }
  id_s
}
