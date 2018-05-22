\name{makeWidgetCmd}

\alias{makeWidgetCmd}

\title{ R functions to build a  GUI window}

\description{
  Function that wraps the result of \code{\link{mapFuncToWidget}}
  as an \R\ function that pop us a widget representing the function.
}

\usage{
  makeWidgetCmd(frmTitle,fun,baseFrame=.TkRoot,STORE=storageName(),
                GRAB=TRUE,SINGLE.EVAL=FALSE)
}

\arguments{
  \item{frmTitle}{ title of the GUI window.}
  \item{fun}{ function to map.}
  \item{baseFrame}{ \code{\link{tcltk}} parent frame of the GUI window
    for the function \code{fun}.}
  \item{STORE}{ A string. Name of the place where to store details needed
    by the GUI to perform the execution.}
  \item{GRAB}{ Logical. When \code{TRUE} disable input in any other window.}
  \item{SINGLE.EVAL}{ Logical. When set to \code{TRUE} it avoids return any 
      value till computation is really finished.}
}

\details{
  The main use of this function is to obtain a function that called
  creates a widget that allows the parameter input and execution of
  function \code{fun}. It also adds a \bold{Quit} \code{fun} function
  to close the widget.
}  

\value{
  This function returns an \R\ function.
}

\author{
  Jorge Luis Ojeda Cabrera (\email{jojeda@unizar.es}).
}

\seealso{
  \code{\link{miniGUI}}, \code{\link{mapFuncToWidget}},
  \code{\link{tcltk}}.
}

\examples{
  require(tcltk)
  ##
  ##  a simple example
  ## 
  g <- function(a=1,b=rnorm) {cat("--g--");paste("g(a,b)=",a+b(a))}
  h <- function(a=1,b=3,c=3) {cat("--h--");paste("h(a,b,c)=",a+b+c)}
  ## create functions
  gg <- makeWidgetCmd("Hay it is g !!",g,GRAB=FALSE)
  hh <- makeWidgetCmd("Hay h here !!",h,GRAB=FALSE)
  ## calling them
  gg()
  cat("\nClose it before calling hh(), they sharer parameters a and b!!")
  hh()
  ##
  ##  simple example(continuation)
  ##
  ## to be able to use both at the same time:save info for h in other place 
  hh <- makeWidgetCmd("Hay h here !!",h,STORE="h")
  gg()
  hh()
}


\keyword{
  utilities
  misc
}
