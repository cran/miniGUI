\name{mapFuncToWidget}

\alias{mapFuncToWidget}

\title{ Map R functions to a GUI window}

\description{
  Function map a large class of \R functions onto a set of
  \code{\link{tcltk}} widgets that allows the input of its
  parameter.
}

\usage{
  mapFuncToWidget(f,frm,bttLabel="OK",STORE="ff",callSubst="mini GUI call")
}

\arguments{
  \item{f}{ Function to map. }
  \item{frm}{ \code{\link{tcltk}} frame to place the GUI window. }
  \item{bttLabel}{ execution button label. }
  \item{STORE}{ A string. Name of the place where to store details needed
    by the GUI to perform the execution.}
  \item{callSubst}{string to set \code{call} attribute/slot in some of the
    R computations results.}
}

\details{
  This function returns a frame which contains pairs of \code{\link{tcltk}}
  labels and text entry (or any other tktcl widget that allows to input
  values) and a button. In this way, this functions maps an \R\ function
  \code{f} into a GUI window that allows its computation. Therefore, it
  provides a map from the a set of \R\ function onto some class of GUI
  windows.
  
  Usually, the way function is executed provides with nasty and long
  \code{call} attributes, \code{call} parameter substitute these
  allowing a much more comfortable output.
  
  The string provided by \code{STORE} is used to store the function
  arguments in the list \code{miniGUIData}, enabling in this way the
  computation of the function.
}

\value{
  This function returns a \code{\link{tcltk}} frame (an object created
  with \code{tkframe}).
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
  ## a window for lm
  ## 
  ## create some data(in the global environment)
  n <- 100
  d <- data.frame(x=runif(n))
  d$z <- 0.5 * rnorm(n)
  d$y <- 2 * d$x + d$z
  ## create a tcltk frame and give it a title
  frm <- tktoplevel()
  tkwm.title(frm,"mapFuncToWidget for lm")
  ## create the GUI window map of lm
  mapFuncToWidget(lm,frm)
  ## ...you may close the window


  ##
  ## a window for T tests
  ##
  myTtest <- function(x,y,mu=0) return( t.test(x=x,y=y,mu=mu) )
  ## create a tcltk frame and give it a title
  frm <- tktoplevel()
  tkwm.title(frm,"mapFuncToWidget for T tests")
  ## create the GUI window map of lm
  mapFuncToWidget(myTtest,frm)
  ## ...you may close the window


  ##
  ##  a simple example
  ## 
  g <- function(a=1,b=rnorm) {cat("--g--");paste("g(a,b)=",a+b(a))}
  h <- function(a=1,b=3,c=3) {cat("--h--");paste("h(a,b,c)=",a+b+c)}
  ## create a tcltk frame and give it a title
  frm <- tktoplevel()
  tkwm.title(frm,"mapFuncToWidget for g")
  ## create the GUI window map of g
  mapFuncToWidget(g,frm)
  ## ...you may close the window


  ##
  ##  the ue of STORE
  ## 
  frm <- tktoplevel()
  tkwm.title(frm,"another map")
  mapFuncToWidget(g,frm,bttLabel="press me !!!")
  ## ... and if you do not close the window
  mapFuncToWidget(h,frm,bttLabel="exec h !!!",STORE="fff")
  ## ...STORE should be added because g and h shares parameter
  ## names a and b
  ## now you may close the window


  ##
  ## a more involved example(see miniGUI widgets)
  ##
  ## some kernels
  kernels <- list(
      gaussK=function(x) dnorm(x,0,1),
      SqK=function(x) ifelse( abs(x) <= 1 , 1 , 0 ),
      EpaK=function(x) ifelse( abs(x) <= 1 , 3/(4*sqrt(5))*(1 - x^2/5) , 0 ),
      TrianK=function(x) ifelse( abs(x) <= 1 , (1 - abs(x)) , 0 )
  )
  ## how to compute the density at x
  prDensEst <- function(x,dat,h,K) mean( K((x-dat)/h) ) / h
  ## the fucntion to appear in the frame
  prDensCurvEst <- function(datos,
     bandwidth=miniGUIscale(from=.05,to=3,by=.05),
     Kernel=miniGUImenusel(c('"gaussK"','"SqK"','"EpaK"','"TrianK"','"QuartK"'))
  )
  {
    n <- length(datos)
    Kernel <- kernels[[Kernel]]
    f <-  function(x) sapply(x,function(x) prDensEst(x,datos,bandwidth,Kernel))
    xeval <- seq(min(datos),max(datos),len=100)
    ##plot pts in x axis
    plot(datos,rep(0,n),pch="+",ylim=c(0,1.25*max(f(xeval))),
         ylab="dens.",main="Density")
    curve(f,add=T)
    return(f)
  }
  formals(prDensCurvEst)$bandwidth <- quote(miniGUIscale(from=.05,to=3,by=.05))
  formals(prDensCurvEst)$Kernel <- quote(
        miniGUImenusel(c('"gaussK"','"SqK"','"EpaK"','"TrianK"','"QuartK"'))
      )
  frm <- tktoplevel()
  tkwm.title(frm,"mapFuncToWidget for f")
  aaa <- mapFuncToWidget(prDensCurvEst,frm)
}

\keyword{
  utilities
  misc
}
