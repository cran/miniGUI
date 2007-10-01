# miniGUI.R
#   Intento de GUI
# NOTAS:
#  - Main procedure and idea:
#    First create menu command functions using 'makeWidgetCmd', afterwards 
#    add these command functions using 'addMenusCmd'.
#  - make invisible 'miniGUIData'.
#  - the flag WRAPFUN=T does not work properly.
# ERRORES:


##
##  load tcltk
##
require(tcltk) || stop("tcltk support is absent")





##
##  Some inits...	
##
miniGUIData <- list()
miniGUIans <- NA




##
##  some evaluation procedures
##
miniGUIcallEval <- function(f,p)
## ok
# f = function to evaluate.
# p = params.
# RETURN:
#   The evaluation of f on params p (string) using envir of f
{
  ## cuidado, el valor de los argumentos se eval. en environment(f) !!
  do.call( f,
           lapply(p,function(x) eval(parse(text=x),envir=environment(f))),
           envir=environment(f)
  )
}


## miniGUIcallEvalB <- function(f,p)
## ## ok
## # f = function to evaluate.
## # p = params.
## # RETURN:
## #   The call, with function f. Doest not work for real calls
## {
##   ## cuidado, el valor de los argumentos se eval. en donde se llama !!
##   do.call( f,
##            lapply(p,function(x) eval(parse(text=x))),
##            envir=environment(f)
##   )
## }


##  set miniGUI evaluation procedure
miniGUIeval <- miniGUIcallEval


##  set miniGUI output procedure
miniGUIoutput <- function(x,mess="\nminiGUI output: \n")
{
  cat(mess)
  print(x)
}




##
##  Creando cosas
##
miniGUIgetFormals <- function(f)
##
# RETURN:
#   Filters arguments and names. By the moment looking for ellipsis.
{
  res <- formals(f)
  ## avoid ellipsis param
  res <- res[names(res)!="..."]
  ## avoid arguments with ellipsis params
  elFun <- grep("[.][.][.]",as.character(res))
  if(length(elFun)>0)
    res <- res[-elFun]    
  return( res )
}


mapFuncToWidget <- function(f,frm,call="mini GUI call",
                               bttLabel="OK",STORE="ff")
## ok
# f = function to display(params are labels and entry fields).
# frm = a frame where toplay trhe display.
# bttLabel = button label"OK",
# STORE = a slot in miniGUIData where to store function param vals.  
#   Makes window widget to permform function computation
{
  ff <- miniGUIgetFormals(f)
  miniGUIData[[STORE]] <<- as.list(ff)
  argsFrame <- tkframe(frm,borderwidth=2)

  ##  tkgrid stuff
  fm <- tkframe(argsFrame, relief="groove", borderwidth=2)
  for(i in names(ff))
  {
    ## get parama value
    par <- deparse(ff[[i]])
    parEval <- eval(parse(text=par))  ##TODO I think we do not need this
    ## select input method widget
    if( is.miniGUIwidget(parEval) ){
      miniGUIData[[STORE]][[i]] <<- tclVar( par )
      imw <- parEval$widget(fm,STORE,i)  
    }else{ ## any other widget should be a text entry
      miniGUIData[[STORE]][[i]] <<- tclVar( par )
      imw <- miniGUIdefaultEntry(fm, textvariable=miniGUIData[[STORE]][[i]])
    }
    ## try to add, if not possible raise error
    tkgrid(tklabel(fm, text=i), tklabel(fm, text="="),imw)
  }
  ## add button frame
  mainJob <- function (...)
  {
    miniGUIans <<- miniGUIeval(f,lapply(miniGUIData[[STORE]],tclvalue))
    if("call" %in% names(miniGUIans)) miniGUIans$call <- call
    ## show result
    miniGUIoutput( miniGUIans )
  }
  tkgrid(tkbutton(fm,text=bttLabel,command=mainJob))
  tkpack(fm,fill="x",expand=TRUE)
  tkpack(argsFrame)
  return(argsFrame)
}




makeWidgetCmd <- function(frmTitle,fun,baseFrame=.TkRoot,STORE="ff",GRAB=TRUE)
## ok
# frmTitle = frame title.
# fun = function to make menu command.
# baseFrame = base frame, if not suppied, it creates a stand alone window.
# STORE = Where to store call params
#   Makes menu command.
{
  ## to avoid lazy eval
  fun 
  frmTitle
  ## real stuff
  res <- function()
  {
    frm <- tktoplevel(baseFrame)
    if(GRAB)tkgrab(frm) ## ensable input only in this frame(disable in main)
    tkwm.title(frm,frmTitle)
    mapFuncToWidget(fun, frm, paste("mini GUI:",frmTitle),"OK", STORE)
    quitCmd <- function()
    {
      ## Remove function storage from miniGUIData
      miniGUIData[[STORE]] <<- NULL
      ## When destroying, main frame is again enabled(ungrabbed !!)
      tkdestroy(frm)
    }
    tkpack( tkbutton(frm,text=paste("Quit",frmTitle),command=quitCmd) )
  }
  return(res)
}




addMenusCmd <- function(cmdFuns,baseFrame)
## ok
# cmdFuns = command functions to add to menu
# baseFrame = base frame
#   Makes menu command.
{
  if(!is.null(cmdFuns))
  {
    opsMenu <- tkmenu(tkmenu(baseFrame),tearoff=TRUE)
    for(i in names(cmdFuns))
    {
      tkadd(opsMenu,  "command", label=i, command=cmdFuns[[i]])
    }
    tkpopup(opsMenu,tkwinfo("pointerx", baseFrame), 
        	    tkwinfo("pointery", baseFrame))
  }
}








##
##  mini GUI 
##
miniGUIffff <- NA
miniGUI <- function(mainFrameFun,opFuns=NULL,title="mini GUI",
                    init=function() {},WRAPFUN=TRUE)
## ok
#  mainFrameFun = function to display(params are labels and entry fields) on
#    the main window frame or NULL.
#  opFuns= List of functions to add in the menu Ops
#  title = main window frame title
#  init = an init function to perform things after the setup. It may assume
#     miniGUIBase existence.
#  WRAPFUN = ??
#    Creates the gui 
{
  ##	tcltk draw main window
  miniGUIBase <<- tktoplevel()
  tkwm.title(miniGUIBase,title)

  ##Some inits...
  init()

  printGUIAns <- function(...){
    print(miniGUIans)
  }
  quit <- function(...){
    tkdestroy(miniGUIBase)
  }
  doNothing <- function(...){
  }
  showGuiData <- function(...){
    res <- NULL
    if(length(miniGUIData)==0)
      cat('\nNo data found.')
    else{
      for(n in names(miniGUIData))
      res <- rbind(res,cbind(CLASS=class(miniGUIData[[n]]),
                             TYPE=typeof(miniGUIData[[n]])))
      rownames(res) <- names(miniGUIData)
      cat("\nMini-GUI data:\n")
      print(res)
    }
  }

  ##	file Menu function
  fileMenuCmd <- function() 
  {
    fileMenu <- tkmenu(tkmenu(miniGUIBase),tearoff=TRUE)
    tkadd(fileMenu, "command", label="GUI data", command=showGuiData)
    tkadd(fileMenu, "command", label="GUI ans.", command=printGUIAns)
    tkadd(fileMenu, "command", label="Quit", command=quit)
    tkpopup(fileMenu, 	tkwinfo("pointerx", miniGUIBase), 
			tkwinfo("pointery", miniGUIBase))
  }
  
  ##	ops Menu function
  if(WRAPFUN) ##when true this does not work, guess it has to do with envirs
  {
    miniGUIffff <<- opFuns
    for(nf in names(opFuns))
      miniGUIffff[[nf]] <<- makeWidgetCmd(nf,opFuns[[nf]],miniGUIBase)
  }else{
    miniGUIffff <<- opFuns
  }
  opsMenusCmd <- function() addMenusCmd(miniGUIffff,miniGUIBase) 

  ##	adds menus
  baseMenu <- tkmenu(tkmenu(miniGUIBase))
  tkadd(baseMenu, "command", label="Basics", command=fileMenuCmd)
  if(!is.null(opFuns))
    tkadd(baseMenu, "command", label="Ops", command=opsMenusCmd)
  tkconfigure(miniGUIBase, menu=baseMenu)

  ##    add labels and fields for mainFrameFun on frame miniGUIBase
  if( is.function(mainFrameFun) )
    mapFuncToWidget(mainFrameFun,miniGUIBase,NULL,"do Job",STORE="mp")
  
  ##    return
  return(NA)
}
# miniGUIwidget.R
#   some predefined miniGUI widgets, of course made using tcltk
# NOTAS:
#  - A miniGUIwidget is a function that should return a function
#  of a frame and a variable name . The frame is the parent frame 
#  of the widget, while the variable whose name is given should
#  exists in the .GlobalEnv and it will be the variable that will
#  reflect the widget value.
# ERRORES:


##
##  miniGUIwidget reckon
##
is.miniGUIwidget <- function(obj) "miniGUIwidget" %in% class(obj)



##
## basic GUI data entry 
##
miniGUIdefaultEntry <- tkentry
miniGUIentry <- function(x)
##
# x = init value
{
  res <- list(widgetType="miniGUIentry",
              widget=function(FRAME,STORE,VAR)  {
                miniGUIData[[STORE]][[VAR]] <<- tclVar( x )
                res <- tkentry(FRAME,textvariable=miniGUIData[[STORE]][[VAR]])
                return( res )
              },  
              x=x)
  class(res) <- c(class(res),"miniGUIwidget")
  return( res )
}


##
## scale GUI data entry
##
miniGUIscale <- function(from,to,by)
##
# from, to, by = from which value, to which value, by such increment.
{
  res <- list(widgetType="miniGUIscale",
              widget=function(FRAME,STORE,VAR)  {
                miniGUIData[[STORE]][[VAR]] <<- tclVar()
                res <- tkscale(FRAME,label="",from=from,to=to,resolution=by,
                           orient="horizontal",
                           variable=miniGUIData[[STORE]][[VAR]])
                return( res )
              },
              from=from,to=to,resolution=by)
  class(res) <- c(class(res),"miniGUIwidget")
  return( res )
}


##
## menu selection GUI data entry
##
## will use ttkcombobox(tt,textvariable=a,values=v) when available
miniGUImenusel <- function(xx)
##
# x = vector of mode numeric or character with available values.
#     x[[1]] is taken as the default value. Logicals should be
#     used as c("T","F").
{
  ## to avoid lazy
  xx
  ## normal stuff
  res <- list(widgetType="miniGUImenusel",
              widget=function(FRAME,STORE,VAR)  {
                ttk85 <- as.character(tcl("info", "tclversion")) >= "8.5"
                if(ttk85) {
                  miniGUIData[[STORE]][[VAR]] <<- tclVar(xx[[1]])
                  res <- ttkcombobox(parent=FRAME,
                            textvariable=miniGUIData[[STORE]][[VAR]],values=xx)
                }else{
                  x <- "Tcl vers. < 8.5, ttkcombobox not available."
                  miniGUIData[[STORE]][[VAR]] <<- tclVar(xx)
                  res <- tkentry(FRAME,textvariable=miniGUIData[[STORE]][[VAR]])
                }
                return( res )
              },
              values=xx)
  class(res) <- c(class(res),"miniGUIwidget")
  return( res )
}
# myPlugins.R
#   Algunos plugins
# NOTAS:
# ERRORES:


## doNothingPlugin <- function(a)
## ##
## {
##   cat("\ndo nothing ",a)
## }


evalPlugin <- function(ev)
##
#  As objects are evaluated before giving them to functions in
#  environment(f), that's all we need.
{
  return( ev )
}






## miniGUIAnsAssPlugin <- function(miniGUIAnsTo)
## ##
## {
##   x <- deparse(substitute(miniGUIAnsTo))
##   if(!(x==""))
##   assign(x,miniGUIans,pos=1)
## }

## lessPlugin <- function(what=miniGUIData)
## ##
## {
##   page(what,"print")
## }

## showCallPlugin <- function(f,a)
## ##
## {
##   call(f,a)
## }

