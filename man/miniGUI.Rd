 \name{miniGUI}

\alias{miniGUI}

\title{ Simple R GUI}

\description{
  Function to create a simple Graphical User Interface based
  on \R\ functions based on \code{\link{tcltk}} package.
}

\usage{
  miniGUI(mainFrameFun,opFuns=NULL,title="mini GUI",init=function() {},
  WRAPFUN=TRUE)
}

\arguments{
  \item{mainFrameFun}{ A function to display (params are labels
    and entry fields) in the main GUI window.}
  \item{opFuns}{ Named list of functions to add in the GUI menu \bold{Ops}.}
  \item{title}{ Main window GUI title.}
  \item{init}{ Function to call before the GUI setup.}
  \item{WRAPFUN}{ when TRUE, automatic \code{\link{tcltk}} widget is
    built for the functions in \code{opFuns}.}
}

\details{
  \code{miniGUI} pops up a window widget with a menu bar containing
  two menus named \bold{Basics} and \bold{Ops} from which different 
  functionality may be addressed during a \code{miniGUI} session. The 
  menu \bold{Basics}  is used to request general purpose task during
  the session (like quitting), while \bold{Ops} is usually where
  more specific tasks, those the GUI is devoted to and that are given
  in \code{opFuns} are grouped. When a menu item from \bold{Ops} is
  selected a new window widget pops us reflecting all the parameters
  the function selected has, so that the user can fill text entries or
  set up the value for such parameters. 

  \code{{init}} can be used to add initialization and checking
  commands to the GUI. This function is executed before any other
  command.
  
  When \code{WRAPFUN} is \code{FALSE} no \code{\link{tcltk}} widget
  is created for the functions in \code{opFuns}, allowing them to
  build their own widget. Do not use it unless functions encode
  its own \link{tcltk}, having into account the internals of the
  package to setup in a proper way the GUI for that specific function.

}

\value{
  \code{miniGUI} function returns nothing. Nevertheless, the results of
  the execution of the different functions called during the \code{miniGUI}
  session are available by means of the \code{miniGUIans} object, and
  also by means of the \bold{GUI ans.} entry in the menu \bold{Basics}.
  
}


\author{
  Jorge Luis Ojeda Cabrera (\email{jojeda@unizar.es}).
}

\seealso{
  \code{\link{miniGUI}}, \code{\link{makeWidgetCmd}},
  \code{\link{addMenusCmd}}, \code{\link{tcltk}}.
}

\examples{
  require(tcltk)
  ##
  ## a simple example 
  ##
  fs <- list(
      f=function(a=1) {cat("--f--");paste("f(a)=",a)},
      g=function(a=1,b=rnorm) {cat("--g--");paste("g(a,b)=",a+b(a))},
      h=function(a=1,b=3,c=3) {cat("--h--");paste("h(a,b,c)=",a+b+c)}
    )
  ## evalPlugin is provided by the package
  miniGUI(evalPlugin,opFuns=fs)

  ##
  ## an example with lm and glm functions
  ##
  ## create some data(in the global environment)
  n <- 100
  d <- data.frame(x=runif(n))
  d$z <- 0.5 * rnorm(n)
  d$y <- 2 * d$x + d$z
  ## makes a wrapper to access t.test
  myTtest <- function(x,y,mu=0) return( t.test(x=x,y=y,mu=mu) )
  ## call miniGUI with myTtest, lm and glm functions
  miniGUI(evalPlugin,opFuns=list("T test"=myTtest,"Lin. Mod."=lm,glm=glm))
  ## try menu "T test" only setting up x

  ##
  ## an example with WRAPFUN set to FALSE
  ##
  gfs <- list()
  for(i in names(fs))
  {
     ## create GUI for fs[[i]] using miniGUIBase
     gfs[[i]] <- makeWidgetCmd(i,fs[[i]],miniGUIBase)
  }
  miniGUI(evalPlugin,opFuns=gfs,WRAPFUN=FALSE) 
}




\keyword{
  utilities
  misc
}
