# write fasta to file
write2fas <- function(data, file){
  fasta_file <- NULL
  for (i in 1:length(data)){
    if (class(data[[i]])=="SeqFasta"){
      seq_title <- attr(data[[i]], "name")
    }
    else if (class(data[[i]])=="SeqGb"){
      seq_title <- paste(">", attr(data[[i]], "species"), attr(data[[i]], "accession"), attr(data[[i]], "seqlength"), attr(data[[i]], "definition"), sep= " ")
    }
    seq_main <- paste(data[[i]], sep="", collapse="")
    fasta_main <- paste(seq_title, "\n", seq_main, sep="", collapse="")
    fasta_file <- append(fasta_file, fasta_main)
  }
  write(fasta_file, file=file, ncolumns=1, sep='')
}
