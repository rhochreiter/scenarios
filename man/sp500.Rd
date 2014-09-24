\name{sp500}
\alias{sp500}
\docType{data}
\title{
  Standard & Poors 500 Index
}
\description{
  Standard & Poors 500 Index (3.1.2009-30.6.2014)
}

\usage{data(sp500)}

\format{
  An xts object with daily OHLC values of the Standard & Poors 500 Index from January 3rd, 2007 to June 30th, 2014.
}

\source{
  \url{http://www.quandl.com/}
  \url{http://finance.yahoo.com/}
}

\references{\url{http://www.github.com/rhochreiter/scenarios/}}

\examples{
  data(sp500)
  
  # install.packages("Quandl", "quantmod")
  # library(Quandl)
  # library(quantmod)
  # sp500 = Quandl("YAHOO/INDEX_GSPC", start_date="2007-01-01",end_date="2014-06-30", type="xts")
  # save(sp500, file="sp500.rda")  
}

\keyword{datasets}
