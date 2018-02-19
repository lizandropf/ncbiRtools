read_fasta <- function (idfile){
  idfile <- readLines(idfile, n=-1L, warn=FALSE)
  f_nam <- grep('^>', idfile, value=FALSE)
  f.r.file <- list()
  if (length(f_nam)==0){
    cat("Not a fasta file")
  }
  else if (length(f_nam)>1){
    for (i in 1:length(f_nam)){
      if (f_nam[[i]] == f_nam[length(f_nam)]){
        f.r.file [[i]] <- unlist(strsplit(idfile[(f_nam[[i]]+1):(f_nam[[i]]+1)], ""))
        attributes(f.r.file[[i]]) <- list(name=idfile[f_nam[[i]]], Annot=idfile[f_nam[[i]]], class = "SeqFastaDNA")
      }
      else if (f_nam[[i]] < f_nam[length(f_nam)]){
        f.r.file [[i]] <- unlist(strsplit(idfile[(f_nam[[i]]+1):(f_nam[[i+1]]-1)], ""))
        attributes(f.r.file[[i]]) <- list(name=idfile[f_nam[[i]]], Annot=idfile[f_nam[[i]]], class = "SeqFastaDNA")
      }
    }
  }
  else if (length(f_nam)==1){
    f.r.file <- unlist(strsplit(idfile[(f_nam+1):(length(idfile)-1)], ""))
    attributes(f.r.file) <- list(name=idfile[f_nam[[1]]], Annot=idfile[f_nam[[1]]], class = "SeqFasta")
  }
  f.r.file
}
