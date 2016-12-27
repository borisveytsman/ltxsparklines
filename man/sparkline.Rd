\name{sparkline}
\alias{sparkline}
\title{
Create a Sparkline
}
\description{
The function creates a TeX expression for a sparkline from the data supplied.
}
\usage{
sparkline(x = NULL, y = NULL, xspikes = NULL, yspikes = NULL,
          xdots = NULL, ydots = NULL, dotcolor = NULL,
          width = getOption("ltxsparklines.width"),
          rectangle = c(NA, NA),
          xlim = c(NA, NA), ylim = c(NA, NA),
          na.rm = getOption("ltxsparklines.na.rm"),
          bottomline = getOption("ltxsparklines.bottomline"),
          bottomlinelength = NA,
          startdotcolor = getOption("ltxsparklines.startdotcolor"),
          enddotcolor = getOption("ltxsparklines.enddotcolor"))
}
\arguments{
  \item{x}{
    The data for sparkline.  If both \code{x} and \code{y} are present,
    they must be vectors of the same length, and interpreted as x and y
    coordinates for a sparkline.  If only \code{x} or \code{y} is
    present, then the result depends on its class:
    \itemize{
      \item If it is a matrix or data frame, then its first column is
      assumed to be 
      the x coordinate, and its second column is assumed to be the y
      coordinate.
      \item If it is a time series (either \code{ts} or \code{zoo}),
      then it is assumed to be the y coordinate, and \code{time(x)} is
      assumed to be the x coordinate.
      \item Otherwise it is assumed to be the y coordinate, and the
      sequence \code{1:length(x)} is assumed to be x coordinate.
      }
}
  \item{y}{
    See \code{x}.
}
  \item{xspikes}{
    The data for spikes: small bar chart similar to sparklines.  If both
    \code{xspikes} and \code{yspikes} are present, they are x and y
    coordinates for spike.  If only one is present, it is interpreted as
    \code{x} above.
}
  \item{yspikes}{
    See \code{xspikes} above.
}
  \item{xdots}{
    The data for dots: small dot chart similar to sparklines.  If both
    \code{xdots} and \code{ydots} are present, they are x and y
    coordinates for dots.  If only one is present, it is interpreted as
    \code{x} above with one addition: if \code{xdots} is a data frame,
    and has three or more columns, and \code{dotcolor} is not set,
    the third column is interpreted as
    the dot color.  
}
  \item{ydots}{
    See \code{ydots} above
}
  \item{dotcolor}{
    The color of dots as a LaTeX color.  A vector like
    \code{c('red', 'blue', 'green')}.  If it is larger than the number
    of dots, it will be truncated.  If it is shorted than the number of
    dots, it will be recycled.

    If it is not set, and data do not provide it,
    \code{option('ltxsparklines.defaultdotcolor')} is used.
}
  \item{width}{
    The width of the sparline in \code{ex} of the current font.  By
    default 10.  To change the default, use
    \code{option(ltxsparklines.width=...)}. 
}
  \item{rectangle}{
    A vector \code{c(min,max)} for the bottom and top of the background
    rectangle (for example, to show IQR of the data).  If \code{NA}, no
    rectangle is drawn.
}
  \item{xlim}{
    A vector \code{c(min,max)} of the x range of the data.  If any of
    its components is \code{NA}, the default, the actual range is used.
}
  \item{ylim}{
    A vector \code{c(min,max)} of the y range of the data.  If any of
    its components is \code{NA}, the default, the actual range is used.
}
  \item{na.rm}{
    Whether to delete \code{NA} lines from the spark data.  If
    \code{TRUE} (the default), the sparkline is drawn over the gaps in
    the data.  Otherwise gaps in the data lead to gaps in the line, so
    it is presented as a number of chunks.
    Note that continuous chunks of data must have at least two data
    points to be drawn.

    This parameter does not affect spikes or dots.
}
  \item{bottomline}{
    Whether to draw a bottom line.  By default, \code{FALSE}.  To
    change the default, use 
    \code{option(ltxsparklines.bottomline=...)}. 
}
  \item{bottomlinelength}{
    The length of bottom line.  If \code{NA} (the default), the bottom
    line extends to the end ot the spark rectangle.
}
  \item{startdotcolor}{
    The color of the dot at the beginning of the spark line.  If
    \code{NA} (the default), no dot is drawn. To
    change the default, use 
    \code{option(ltxsparklines.startdotcolor=...)}. 
}
  \item{enddotcolor}{
    The color of the dot at the end of the spark line.  If
    \code{NA} (the default), no dot is drawn. To
    change the default, use 
    \code{option(ltxsparklines.enddotcolor=...)}. 
}
}
\details{  
  Most of the parameters of the sparkline are defined by the TeX code.
  Thus all color values must be understandable to LaTeX.  For example,
  you can put in the TeX file

  \preformatted{\definecolor{startdot}{named}{red}}

  and then use it like \code{sparkline(Nile, startdotcolor='startdot')}
}
\value{
  The function returns a string executable by TeX, to be used in
  \preformatted{\Sexpr}, for example
  \preformatted{Nile level changed over the years: \Sexpr{sparkline(Nile)}}.
}
\author{
Boris Veytsman
}
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
\keyword{ hplot }

