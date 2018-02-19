\name{write2fas}
\alias{write2fas}
\title{
Tool for writing fasta files.
}
\description{
This function writes fasta files from data retrieved from GenBank as well as
GenBank files read from disk.
}
\usage{
write2fas(data, file)
}
\arguments{
  \item{data}{a data structure from \code{\link{fetchseq}} or a read GenBank
  file read from disk with \code{\link{read_gb}}.}
  \item{file}{a file name for the file to be written to disk.}
}

\references{
For details on database visit:
https://ncbi.nlm.nih.gov
}
\author{Lizandro N. Peraza-Flores}

\note{It writes a fasta file from one to multiple sequences store in a data
structure recovered from \code{\link{fetchseq}} or \code{\link{read_gb}}.}

\seealso{
\code{\link{searchGB}, \link{fetchseq}, \link{read_fasta}, \link{read_gb}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
