\name{miniGUIinputWidget}

\alias{miniGUIinputWidget}
\alias{miniGUImenusel}
\alias{miniGUIentry}
\alias{miniGUIscale}
\alias{miniGUIdefaultEntry}
\alias{is.miniGUIwidget}



\title{ Entry widgets}

\description{
  Function that builds different input methods.
}

\usage{
  miniGUIentry(x)
  miniGUIscale(from,to,by)
  miniGUImenusel(xx)
}

\arguments{
  \item{x}{ A \R\ symbol, or numerical or character value. It can  also
    be any \R\ expression.}
  \item{from,to,by}{ three numerical values.}
  \item{xx}{ Any vector of mode numeric or character.}
}

\details{
  These functions implements different input methods. In order to
  work these should appear as the default values of parameters
  in the definition of the function whose widget is to be built.
  In this way, the specification of the GUI input method for all
  the parameters can be done in a simple way by means of the
  definition if the function. It is worth mentioning that functions
  defined in this way can use parameters in the ordinary way if a
  value is provided for them. See the examples below.
  
  In order to map a function onto a widget, \code{\link{mapFuncToWidget}}
  uses a \code{\link{tkentry}} that contains the character conversion of
  the default value for that parameter if there exist such a value, or
  that contains nothing there is no such a default value.

  \code{miniGUIdefaultEntry} is the default entry widget, at the moment
  a simple \code{\link{tkentry}}.
  
  \code{miniGUIentry(x)} makes the \code{\link{tkentry}} related to
  the parameter to contain \code{x}. This widget is included as an
  example of the way widget can be added.

  \code{miniGUIscale(from,to,by)} uses \code{\link{tkscale}} to show a
  slider that allows to input numerical values in the range \code{from},
  \code{to} with an increment of \code{by}.

  \code{miniGUImenusel(xx)} uses \code{\link{ttkcombobox}} (needs  Tcl
  version 8.5 or later) to show a menu with entries \code{xx}, a
  character or numerical vector.

  These functions and their implementation show how new input widget can
  be added in a simple way.
  %% should comment about how to include new input method
}  

\value{
  All these functions returns an object \code{miniGUIwidget}, that
  is a list with at least the entry \code{widget} that should be a
  function and any other detail. 

  The function \code{widget} builds an entry widget using
  \code{\link{tcltk}} functions and should return it. This function
  should be defined having three parameters: \code{FRAME}, \code{STORE},
  \code{VAR}. In short, the first one is used by the internal code to
  provide a \code{\link{tcltk}} parent frame, the second to provide a
  place where to save the value of the parameter and the third one is
  used to save the parameter name.

  The implementation details may change in the future.
}

\author{
  Jorge Luis Ojeda Cabrera (\email{jojeda@unizar.es}).
}

\seealso{
  \code{\link{miniGUI}}, \code{\link{makeWidgetCmd}},
  \code{\link{miniGUIgetFormals}}, \code{\link{addMenusCmd}},
  \code{\link{tcltk}}.
}

\examples{
  require(tcltk)
  ##
  ##  simple example 
  ##
  #   ...define a function
  h <- function(a=miniGUImenusel(c(1,5,10)),
		b=miniGUIscale(from=5,to=10,by=2),
		c=miniGUIentry(4),
		d=miniGUImenusel(c("T","F")),
                e
		)
  {
    cat("--h--");paste("h(a,b,c)=",d*(a+b+c))
  }
  ## building it
  hmm <- makeWidgetCmd("Hay !!",h)
  hmm()
  ##
  ##  another example
  ##
  ## create some data(in the global environment)
  n <- 100
  d <- data.frame(x=runif(n))
  d$z <- 0.5 * rnorm(n)
  d$y <- 2 * d$x + d$z
  ## def mylm method
  mylm <- lm
  formals(mylm)$method <- quote( miniGUImenusel(c('"qr"','"model.frame"')) )
  formals(mylm)$x <- quote( miniGUImenusel(c("FALSE","TRUE")) )
  ## add this stuff
  miniGUI(evalPlugin,opFuns=list(mylm=mylm,lm=lm)) 

}


\keyword{
  
  utilities
  misc
}