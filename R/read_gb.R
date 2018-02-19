## Read GB files
read_gb <- function(seq.file){
  seq.file <- readLines(seq.file, n=-1L, warn=FALSE)
  locus.search <- unlist(strsplit(grep("LOCUS", seq.file, value=TRUE), "  "))
  locus.search <- locus.search[locus.search!=""]
  def.search <- sub("DEFINITION *", "", seq.file[(grep("DEFINITION", seq.file)):(grep("ACCESSION", seq.file)-1)])
  def.search <- paste(sub(" {2,}", "", def.search),collapse="")
  source.search <- sub("SOURCE *", "", grep("SOURCE ", seq.file, value=TRUE))
  org.search <- sub(" *ORGANISM *", "", grep("ORGANISM ", seq.file, value=TRUE))
  gb.f <- unlist(strsplit(sub(" *[0-9]* ", "", seq.file[(grep("ORIGIN", seq.file)+1):(grep("//", seq.file)-1)]),""))
  gb.f <- gb.f[gb.f!=" "]
  attributes(gb.f) <- list(seqlength= sub("^ ", "", grep("[0-9]* bp", locus.search, value=TRUE)), seqtype= locus.search[[4]], PLN= sub("^ PLN ", "", grep(" PLN ", locus.search, value=TRUE)), definition = def.search, accession=sub("^ ", "", grep("[A-Z]{2}[0-9]{4,}", locus.search, value=TRUE)), version= sub("^VERSION *", "", grep("VERSION *[A-Z]{2}[0-9]{4,}.[0-9]{1}", seq.file, value=TRUE)),  species= org.search, class="SeqGb", )
  gb.f
}
