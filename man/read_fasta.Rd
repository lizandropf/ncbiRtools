\name{read_fasta}
\alias{read_fasta}
\title{
Tools for reading fasta files to R environment.
}
\description{
This function reads fasta files from disk or those retrieve by
\code{\link{fetchseq}} (within function).
}
\usage{
read_fasta(idfile)
}
\arguments{
  \item{idfile}{file path of fasta file.}
}
\details{It reads fasta files to disk. It is also used internally for function
\code{\link{fetchseq}} to read retrieved fasta files from GenBank.}

\references{
For details on database visit:
https://ncbi.nlm.nih.gov
}
\author{L.N. Peraza-Flores}

\seealso{
\code{\link{searchGB}, \link{fetchseq}, \link{read_gb}, \link{write2fas}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
