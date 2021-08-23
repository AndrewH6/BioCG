#' provides a nice summary and test for a categorical variable by a grouping variable, with optional Fisher's exact test

#' @export

bicatvarsum=function(mat, idcol, varcol, groupcol,doplot=T,myrot=F,mytest=T,check5=F,print5=T,usecolours=NULL){
  #provides a nice summary and test for a categorical variable by a grouping variable, with optional Fisher's exact test
  #assumes that missing data are NA or "NA", all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #checks to see if any counts are too small to exit CaraSpace
  #mat=matrix or dataframe containing the data
  #idcol=column number of the subject ID
  #varcol=column number of the categorical variable you want to summarize
  #groupcol=column number of the grouping variable
  #doplot=T if T, produces plots; change to F to not do plots
  #myrot=F if F, don't rotate the x-axis labels; if T, rotate the x-axis labels
  #check5=T if T, check if any cell counts are less than 5; if F, doesn't do the check
  #print5=F if F, don't print any cell counts that are less than 5; if T, print all cell counts
  #usecolours is a vector of user-supplied colours to use for barcharts (ie, "red" or hex codes)

  BCthresh=5 #threshold for checking what is too small to leave CaraSpace

  check5=F
  print5=T

  #check if there is a small number in a count that we need to worry about having it leave CaraSpace
  nomistable=table(mat[, varcol],mat[,groupcol])
  less5=0
  if(check5==T){
    if(min(nomistable)<BCthresh) {
      less5=1   #flag that there is a small number
      cat("\n\n********* WARNING: A count is less than",BCthresh,"and needs not to leave CaraSpace.\n\n")
    }
  }

  grouplev=dimnames(table(mat[,groupcol]))[[1]] #figure out how many distinct groups
  nlev=length(grouplev)
  usenames=c(names(mat)[varcol],names(mat)[groupcol])
  #cat("1 HERE\n")
  mylas=1;myfactor=1; if (nlev>5) {mylas=2;myfactor=0.55}

  if(less5==0 || print5==T){
    if(mytest==T){cat("NOTE: Tests assume independent data (ie distinct subjects) and wouldn't be valid if multiple data points per subject.")}
    if(nlev>1){
      mytwoframe = data.frame(var1 = mat[, varcol], var2 = mat[, groupcol])
      if(nlev>5 && nlev>length(dimnames(table(mat[,varcol]))[[1]])){ #if lots of levels
        mytwoframe=data.frame(var1 = mat[, groupcol], var2 = mat[, varcol])
        usenames=c(usenames[2],usenames[1])
        if(mytest==T){
          CrossTable(mytwoframe[,1], mytwoframe[,2], digits=3, max.width = 4, expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                     prop.t=TRUE, prop.chisq=FALSE, chisq = T, fisher=FALSE, mcnemar=FALSE,
                     resid=FALSE, sresid=FALSE, asresid=FALSE,
                     format="SAS",dnn=usenames)}
        else{      CrossTable(mytwoframe[,1], mytwoframe[,2], digits=3, max.width = 4, expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                              prop.t=TRUE, prop.chisq=FALSE, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
                              resid=FALSE, sresid=FALSE, asresid=FALSE,
                              format="SAS",dnn=usenames)}
        if(nrow(mat)<1000 && mytest==T){
          fisher.test(mytwoframe[,1], mytwoframe[,2], workspace = 200000, hybrid = FALSE,
                      control = list(), or = 1, alternative = "two.sided",
                      conf.int = TRUE, conf.level = 0.95,
                      simulate.p.value = T, B = 2000)
        }
        #colors <- rainbow(length(unique(mytwoframe[,2])))
      }else{#if not that many levels
        if(nlev<5 & length(dimnames(table(mat[,varcol]))[[1]])<5 ){
          if(mytest==T){
            CrossTable(mytwoframe[,1], mytwoframe[,2], digits=3, max.width = 4, expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                       prop.t=TRUE, prop.chisq=FALSE, chisq = T, fisher=FALSE, mcnemar=FALSE,
                       resid=FALSE, sresid=FALSE, asresid=FALSE,
                       format="SAS",dnn=usenames)}
          else{
            CrossTable(mytwoframe[,1], mytwoframe[,2], digits=3, max.width = 4, expected=FALSE, prop.r=TRUE, prop.c=TRUE,
                       prop.t=TRUE, prop.chisq=FALSE, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,
                       resid=FALSE, sresid=FALSE, asresid=FALSE,
                       format="SAS",dnn=usenames)
          }
          if(nrow(mat)<1000 && mytest==T){
            fisher.test(mytwoframe[,1], mytwoframe[,2], workspace = 200000, hybrid = FALSE,
                        control = list(), or = 1, alternative = "two.sided",
                        conf.int = TRUE, conf.level = 0.95,
                        simulate.p.value = F, B = 2000)}
        }
        else{
          twomiss=table(as.factor(mat[,varcol]),as.factor(mat[,groupcol]));
          print(twomiss)
        }
        if(doplot==T){# do a barchart
          colors <- rainbow(length(unique(mytwoframe[,2])))
          if(is.null(usecolours)==F){
            if(length(colours)==length(unique(mytwoframe[,2]))){
              colors=usecolours
            }else{
              cat("\n\nNOTE: usecolours supplied had length",length(usecolours),"but the number needed is",length(unique(mytwoframe[,2])),
                  "so rainbow colours used instead.")
            }  }


          bartable = table(mat[,varcol], mat[,groupcol])
          mylabs=rep(row.names(bartable),each=2)
          mylabs[seq(2,length(mylabs),2)]=""
          bartable=t(bartable)
          maxy=1.1*nrow(mat)
          t1=max(bartable)/maxy
          myi=.05*maxy
          if (max(bartable)<.5*maxy) {maxy=1.1*t1*maxy;myi=.05*maxy}
          bp=barplot(bartable, beside = TRUE, ylab="Frequency",#names.arg=mylabs,
                     xlab=dimnames(mat)[[2]][varcol],main="",ylim=c(0,maxy),las=mylas,cex.names=myfactor,col=colors)  ## plot
          legend("topright", paste(names(mat)[groupcol],rownames(bartable),sep="="), fill = colors, bty = "n",cex=0.75)

          if(length(bp)<20){text(bp, bartable+myi,bartable,xpd=NA)}
        }}
    }
  }else{
    cat("\n\nNOTE: Nothing printed because there was a number <",BCthresh,"and print5=F says not to print out. Can change print5=T to see the full summary.\n\n ")
  }
}
