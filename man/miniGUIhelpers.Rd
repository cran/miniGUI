\name{miniGUIhelpers}

\alias{miniGUIhelpers}
\alias{addMenusCmd}
\alias{miniGUIgetFormals}
\alias{miniGUIcallEval}
\alias{evalPlugin}
\alias{miniGUIoutput}
\alias{miniGUIeval}
\alias{miniGUIans}
\alias{miniGUIData}
\alias{miniGUIffff}

\title{ Utility functions }

\description{
  Some utility functions
  }

\usage{
  addMenusCmd(cmdFuns,baseFrame)
  miniGUIgetFormals(f)
  miniGUIcallEval(f,p)
  evalPlugin(ev)
  miniGUIoutput(x,mess="\nminiGUI output: \n")
}

\arguments{
  \item{cmdFuns}{ A list of \R\ function.}
  \item{baseFrame}{ \code{\link{tcltk}} parent frame of the GUI window
    for the function \code{fun}.}
  \item{f}{ An \R\ function.}
  \item{p}{ a list with all the parameters \code{f} requires.}
  \item{x}{ An \R\ object to print.}
  \item{mess}{ a string with a brief message that is printed before
    \code{x}.}
  \item{ev}{ a string containing an \R\ expression.}
}

\details{
  These functions are internal functions that helps building the
  GUI map. \code{addMenusCmd} adds a menu to the main
  \code{\link{miniGUI}} frame. \code{miniGUIgetFormals} gets the
  parameter list of the function \code{f} filtering ellipsis. 
  \code{miniGUIcallEval} performs the evaluation \code{f} when
  the arguments are set to those of \code{p}. \code{miniGUIoutput}
  is used to print out the result of the computation.
  
  At the present moment, \code{miniGUIeval} is the same as the function
  \code{miniGUIcallEval},  while \code{miniGUIans} is used to store
  the result of the last computation made by a call to any of the
  \code{miniGUI} menu functions or any function widget created with
  by the functions \code{makeWidgetCmd} or \code{mapFuncToWidget}.
  \code{miniGUIData} and \code{miniGUIffff} are used to store information 
  required to compute function widgets.  
  %% add error handling capabilities
}


\author{
  Jorge Luis Ojeda Cabrera (\email{jojeda@unizar.es}).
}

\seealso{
  \code{\link{miniGUI}}, \code{\link{makeWidgetCmd}},
  \code{\link{addMenusCmd}}, \code{\link{tcltk}}.
}


\keyword{
  utilities
  misc
}
