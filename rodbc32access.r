MysqlQueryAccess2007<-function(filename,query){
  tempdir=gsub('\\\\','/',tempdir())
  txt<-paste("if (!'RODBC' %in% installed.packages()) install.packages('RODBC')
             library(RODBC, quietly = TRUE)
             channel<-odbcConnectAccess2007('",filename,"')
             data<-sqlFetch(channel,\"",query,"\")
             save(data,file=paste('",tempdir,"','tempRODBCquery.Rdata',sep='/'))
             close(channel)",sep="")
  
  writeLines(txt,con=paste(tempdir,'RODBCscripttemp.r',sep='/')->tempscript)
  system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ",tempscript))
  tt<-get(load(paste(tempdir,'tempRODBCquery.Rdata',sep='/')))
  return(tt)
}



fn <- "d:/bernd/r/koalaoccupancy/GDA94z55Database2017JUN.accdb"
query <- "Tree_Species"

tl <- MysqlQueryAccess2007(fn, query)

query <- "TREE_MASTER"
tloc <- MysqlQueryAccess2007(fn, query)
