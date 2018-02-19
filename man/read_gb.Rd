\name{read_gb}
\alias{read_gb}
\title{
Tool for reading GenBank sequence files.
}
\description{
This function reads GenBank sequence file.
}
\usage{
read_gb(seq.file)
}
\arguments{
  \item{seq.file}{file path of GenBank sequence file. It is used internally
  by \code{\link{fetchseq}} to read data from retrieved sequences.}
}

\references{
For details on database visit:
https://ncbi.nlm.nih.gov
}
\author{Lizandro N. Peraza-Flores}

\note{It is used internally for reading GenBank data format in function
\code{\link{fetchseq}}.}

\seealso{
\code{\link{searchGB},  \link{fetchseq}, \link{read_fasta}, \link{write2fas}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
