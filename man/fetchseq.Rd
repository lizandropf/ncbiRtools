\name{fetchseq}
\alias{fetchseq}
\title{
Fetch sequences from UIDs recovered from a GenBank search with function searchGB
or Genbank accession numbers in a vector.
}
\description{
This function fetch sequences from the NCBI GenBank into fasta or GenBank files.
The elements can be a result from function searchGB or a vector with UIDs or
GenBank accession numbers.
}
\usage{
fetchseq(ids, db="nuccore", rettype="fasta", retmode="text")
}
\arguments{
  \item{ids}{data recovered from function \code{\link{searchGB}} or a vector
  with UIDs or GenBank accession numbers.}
  \item{db}{The database to retrieve sequences from. It uses UIDs or GenBank
  Accession numbers.
  It can be any of available options in NCBI databases. Set to "nuccore" as
  default. It retrieves data from two databases, genbank (any database) or
  pubmed.}
  \item{rettype}{either "fasta" or "gb". Set to "fasta" as default (best for
  retrieving data from a vector of UIDs or Accession numbers with known taxa).
  "gb" option retrieves more data information for each sequence (recommended
  when retrieving data from a search with function searchGB).
  New options added for db="pubmed", either rettype="docsum" for summary records
  or rettype="abstract" for complete records.}
}

\references{
For details on database visit:
https://ncbi.nlm.nih.gov
}
\author{L.N. Peraza-Flores}

\note{Warning for extensive data retrieving: connection to server would be
interrupted by NCBI. Function offers a 1 seconds delay every 3 records
retrieved.}

\seealso{
\code{\link{searchGB}, \link{read_fasta}, \link{read_gb}, \link{read_PubMedRec}, \link{write2fas}}
}
\examples{
# load example data:
# from a vector of NCBI Accession numbers
seqs_data <- c("KP168671", "EU140072", "AY008665", "EU140235", "EU139932")
retr_seqs <- fetchseq(seqs_data, rettype="fasta")
# from results of function searchGB
sear_seqs <- searchGB("Cattleya crispata", retmax=5)
retr_seqsSgb <- fetchseq(sear_seqs)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
