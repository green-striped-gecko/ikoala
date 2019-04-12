#ikoala_mon_GUI

suppressMessages(library(rmarkdown, quietly=TRUE, warn.conflicts=FALSE))
#### check for pandoc. If not available search on stick!!! otherwise use existing one.
if (!pandoc_available())  Sys.setenv(PATH = paste(paste0(toupper(substr(getwd(),1,2)),"/Rstudio/bin/pandoc"),Sys.getenv("PATH"),  sep=.Platform$path.sep))


suppressMessages(library(R2jags, quietly=TRUE, warn.conflicts=FALSE))
#set jags
#Sys.setenv("JAGS_HOME"= paste0(substr(getwd(),1,2),"/Program Files/JAGS/JAGS-4.3.0"))

suppressMessages(library(xtable, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(pander, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(knitr, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(rgdal, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(plyr, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(psych, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(ggmap, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(RgoogleMaps, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(ggplot2, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(vegan, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(akima, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(reshape2, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(RColorBrewer, quietly=TRUE, warn.conflicts=FALSE))

suppressMessages(library(tcltk, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(tcltk2, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(sp, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(maptools, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(tidyr, quietly=TRUE, warn.conflicts=FALSE))
suppressMessages(library(RODBC, quietly=TRUE, warn.conflicts=FALSE))

koalaGUI <- function(){

  rbvalue <- tclVar("site")
  resf <- tclVar("...")
  repname <- tclVar("ikoala report")
  mat1 <-  c("region","NA","from","NA","to","NA")
  dim(mat1) <- c(2,3)
  pertable= tclArray()
  numberofrows=5 #number of printed rows.
  for (i in 1:nrow(mat1))
  for (j in 1:ncol(mat1))
    pertable[[i-1, j-1]] <- mat1[i, j]
  
  
  
  
  tt <- tktoplevel()
  heading <- tkfont.create(family = "Arial", size = 12, weight = "bold")
  fett <- tkfont.create(family = "Arial", size = 10, weight = "bold")
  
  img <- tclVar()
  ik <-tkimage.create("photo", img, file="images/ikoala.png")
  icon <- tk2ico.create("images/ikoala.ico")
  tk2ico.set(tt, icon)
  rbsite <- tkradiobutton(tt)
  rbregion <- tkradiobutton(tt)

  sf <- tclVar("*.accdb")
  perf <-tclVar("*.csv")
  studyf <- tclVar("*.csv")
  mdistance <- tclVar("501")
  label1 <- tklabel(tt, text=tclvalue(sf),font=fett, fg="green")
  label2<- tklabel(tt, text=tclvalue(perf),font=fett, fg="green")
  label3 <- tklabel(tt, text=tclvalue(studyf), font=fett, fg="green")
  label4 <- tklabel(tt, text=tclvalue(resf),font=fett, fg="green")
  
  
  
  tkconfigure(label1, textvariable=sf)
  tkconfigure(label2, textvariable=perf)
  tkconfigure(label3, textvariable=studyf)
  tkconfigure(label4, textvariable=resf)
  
  tkconfigure(rbsite, variable = rbvalue, value="site")  
  tkconfigure(rbregion, variable = rbvalue, value="region")  
  
  
  tkwm.title(tt,"ikoala")
  sf.entry <- tkentry(tt, textvariable=sf)
  distance.entry <- tkentry(tt, textvariable=mdistance)
  res.entry <- tkentry(tt, textvariable=repname)
  reset <- function() {
    tclvalue(sf) <-"*.accdb"
    tclvalue(perf)<- "*.csv"
    tclvalue(studyf)<-"*.csv"
    tclvalue(repname) <- "ikoala report"
    tclvalue(rbvalue) <- "site"
    tclvalue(resf)<-"..."
  }
  
  
  seldatabase.but <- tkbutton(tt, text="Select data base", command=function() {
    tclvalue(sf)=tk_choose.files(caption="Select data base file", filters = matrix(c("Access database",".accdb","All files","*"), 2,2,byrow = TRUE)) 
    
  })
  
  selperiods.but <- tkbutton(tt, text="Select periods file", command=function() {
    tclvalue(perf)=tk_choose.files(caption="Select periods file", filters = matrix(c("periods file","periods*.csv","All files","*"), 2,2,byrow = TRUE)) 
    mat1 <- read.csv(tclvalue(perf))
    nr <- nrow(mat1)
    nc <- ncol(mat1)
    
    mat1 <- unlist(lapply(mat1, as.character))
    dim(mat1) <- c(nr, nc)
    for (i in 1:nrow(mat1))
      for (j in 1:ncol(mat1))
        pertable[[i, j-1]] <- mat1[i, j]
  })
  

  selstudysite.but <- tkbutton(tt, text="Select study site file", command=function() {
    tclvalue(studyf)=tk_choose.files(default="sites", caption="Select study site file", filters = matrix(c("study site file","sites*.csv","All files","*"), 2,2,byrow = TRUE)) 
  
  })
  
  selresf.but <- tkbutton(tt, text="Select output folder", command=function() {
    tclvalue(resf)=tk_choose.dir(caption="Select output folder") 
    
  })
   
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit <- function() {
    
    render("ikoala_monitoring3.rmd", output_file = paste0(tclvalue(resf),"/",tclvalue(repname),".docx"),word_document(fig_caption = TRUE,  reference_docx = "styles.docx"), params = list(report_name=tclvalue(repname), database=tclvalue(sf), study_periods=tclvalue(perf), study_site =tclvalue(studyf), restricted=tclvalue(rbvalue), max_distance=tclvalue(mdistance), projection="+proj=utm +zone=55 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs", results_folder=tclvalue(resf)), clean = TRUE)
    
    tkmessageBox(type="ok",message=paste("Report has been created. \nCheck file:",tclvalue(repname)," in the folder:",tclvalue(resf),"for the report." ))
 
  }
  submit.but <- tkbutton(tt, text="Create an ikoala report", image=ik, compound="top", command=submit)
  
  
  
#######################
### GUI layout ########
#######################
  tkgrid(tklabel(tt, text="Input", font=heading), padx=10, sticky="w")
    tkgrid(tklabel(tt,text="1. Select database file (accdb)"), seldatabase.but, columnspan=1, padx=10, sticky="w")
  tkgrid( label1, columnspan =1,  padx=20, sticky="w")
 
  tkgrid(tklabel(tt,text="2. Select study periods file (csv)"),selperiods.but,   padx=10, sticky="w")
  tkgrid( label2,columnspan =1,  padx=20, sticky="w")
  
  #table1 <- tkwidget(tt,type =  "table" , variable=pertable)#, rows=numberofrows, cols=3, titlerows=1, selectmode="extended",colwidth=15,background="lightgrey", fg="black")
  
  table1 <- tk2table(tt, variable=pertable, rows=numberofrows, cols=3, titlerows=1, selectmode="extended",colwidth=20,background="lightgrey", fg="black")
  
  tkgrid(table1, columnspan=1,  padx=10)
   
  tkgrid(tklabel(tt,text="3. Specify a study site file     "), selstudysite.but, padx=10, sticky="w")
  
  tkgrid(label3, columnspan =1,  padx=20, sticky="w")
  
 
  tkgrid(tklabel(tt,text="4. Specify if the detailed report is based on the coordinates \n given by the study site file (site) or region (region)\n in the periods file"),  padx=10, sticky="w")
  tkgrid(tklabel(tt, text="site"), rbsite)
  tkgrid(tklabel(tt, text="region"), rbregion)
  
  tkgrid(tklabel(tt,text="5. Maximum distance to combine sites for monitoring\n (best to set to half grid distance+1): "), distance.entry, padx=10, sticky="w")
  tkgrid(tklabel(tt, text="Output", font=heading), pady=5, padx=10, sticky="w")
  tkgrid(tklabel(tt,text="6. Specify report title"), res.entry , padx=10, sticky="w")
  tkgrid(tklabel(tt,text="7. Select output folder \n (must be writable)"), selresf.but, columnspan=1, padx=10, sticky="w")
  tkgrid( label4, columnspan =1,  padx=20, sticky="w")
  
  tkgrid(submit.but, reset.but,  pady= 10, padx= 10)
  cat("We hope you enjoy using iKoala!\n")
  tkwait.window(tt)
}

koalaGUI ()
