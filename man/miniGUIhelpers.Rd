\name{miniGUIhelpers}

\alias{miniGUIhelpers}
% \alias{addMenusCmd}
\alias{miniGUIgetFormals}
\alias{miniGUIcallEval}
\alias{miniGUIeval}
\alias{miniGUIoutput}
\alias{setMiniGUIData}
\alias{setMiniGUIans}
\alias{getMiniGUIData}
\alias{getMiniGUIans}
\alias{miniGUIEnvir}
\alias{storageName}
% \alias{miniGUIffff}

\title{ Utility functions }

\description{
  Some utility functions that are not exported
  }

\usage{
  miniGUIgetFormals(f)
  miniGUIcallEval(f,p,e)
  miniGUIoutput(x,mess="\nminiGUI output: \n")
}

\arguments{
  \item{f}{ An \R\ function.}
  \item{p}{ a list with all the parameters \code{f} requires.}
  \item{e}{ environment where the parameters \code{p} of \code{f} are evaluated. By default \code{.GlobalEnv} .}
  \item{x}{ An \R\ object to print.}
  \item{mess}{ a string with a brief message that is printed before
    \code{x}.}
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
  \code{miniGUIcallEval},  while \code{miniGUIEnvir} is used to store
  \code{miniGUI} internal data. In particular, \code{miniGUIEnvir$miniGUIans}  
  stores the result of the last computation made by a call to any of the
  \code{miniGUI} menu functions or any function widget created with
  by the functions \code{makeWidgetCmd} or \code{mapFuncToWidget}. On 
  the other hand \code{miniGUIEnvir$miniGUIData} stores information and 
  parameters required to compute function widgets. In ordet to do so, 
  function \code{storageName} is used to avoid name collisons. The functions 
  \code{setMiniGUIData} \code{setMiniGUIans}, \code{getMiniGUIData}, 
  \code{getMiniGUIans} are used to set and get data from 
  \code{miniGUIEnvir$miniGUIData} and \code{miniGUIEnvir$miniGUIans} resp..
  %% add error handling capabilities
}


\author{
  Jorge Luis Ojeda Cabrera (\email{jojeda@unizar.es}).
}

\seealso{
  \code{\link{miniGUI}}, \code{\link{makeWidgetCmd}},
  \code{\link{mapFuncToWidget}}, \code{\link{tcltk}}.
}


\keyword{
  utilities
  misc
}
