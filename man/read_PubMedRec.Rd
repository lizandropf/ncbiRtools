\name{read_PubMedRec}
\alias{read_PubMedRec}
\title{
Tools for reading Pubmed files into R environment.
}
\description{
This function reads pubmed text files from disk or those retrieve by
\code{\link{fetchseq}} (within function).
}
\usage{
read_PubMedRec(idfile, rettype)
}
\arguments{
  \item{idfile}{file path of pubmed text file.}
  \item{rettype}{either "docsum" or "abstract" for pubmed text file.}
}
\details{It reads pubmed text files to disk. It is also used internally for function
\code{\link{fetchseq}} to read retrieved pubmed record files from GenBank.}

\references{
For details on database visit:
https://ncbi.nlm.nih.gov
}
\author{L.N. Peraza-Flores}

\seealso{
\code{\link{searchGB}, \link{fetchseq}, \link{read_fasta}, \link{read_gb}, \link{write2fas}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
