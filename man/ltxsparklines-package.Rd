\name{ltxsparklines-package}
\alias{ltxsparklines-package}
\alias{ltxsparklines}
\docType{package}
\title{
\packageTitle{ltxsparklines}
}
\description{
\packageDescription{ltxsparklines}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ltxsparklines}
\packageIndices{ltxsparklines}

There are several packages that produce sparklines in R: SparkTable by
Alexander Kowarik, Bernhard Meindl, Matthias Templ
(https://CRAN.R-project.org/package=sparkTable),  sparklines by Bart
(https://github.com/Bart6114/sparklines).  However, they either provide
HTML-only output, or require Java libraries.  This is a lightweight and
easy to use package for LaTeX users.

The package provides one function, \code{\link{sparkline}}, which
outputs LaTeX code compatible with LaTeX package \code{sparklines}
(Andreas Loeffer and Dan Luecking, http://www.ctan.org/pkg/sparklines). 
}
\author{
Boris Veytsman

Maintainer: <borisv@lk.net> 
}
\references{
  Andreas Loeffer and Dan Luecking, Sparklines, 2014,
  http://www.ctan.org/pkg/sparklines
}
\keyword{ package }

\examples{
\dontrun{
sparkline(x=c(1,3,6),
          y=c(5,8,9),
          startdotcolor='blue',
          enddotcolor='red')
}
\dontrun{
sparkline(Nile)
}
}
