\name{searchGB}
\alias{searchGB}
\title{
Search GenBank database for sequences.
}
\description{
This function searches the GenBank database for retriving sequences of taxa.
}
\usage{
searchGB(term, db="nuccore", retmax=20)
}
\arguments{
  \item{term}{a search term to find sequence data related to it.}
  \item{db}{a database to search within, default set to "nuccore", the main
  sequence database.}
}

\references{
For details on database visit:
https://ncbi.nlm.nih.gov
}
\author{Lizandro N. Peraza-Flores}

\note{Depending on search terms, the data retrieved cannot be of the same taxon
as the intended search term.
It also retrieve the number of records associated to the search. If retmax do
not include all records this information can be helpful to retrieve all data
associated to the search term.}

\seealso{
\code{\link{fetchseq}, \link{read_fasta}, \link{read_gb}, \link{write2fas}}
}
\examples{
# load example data:
sear_seqs <- searchGB("Cattleya crispata", retmax=5)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
