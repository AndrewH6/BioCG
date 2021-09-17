#' provides a nice summary for a categorical variable, with optional barchart

#' @export

BioCG_catvarsum=function(mat, idcol, varcol, doplot=T,myrot=F,myuni=T,check5=F,print5=T,usecolours=NULL) {
  #provides a nice summary for a categorical variable, with optional barchart
  #assumes that missing data are NA or "NA", all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #checks to see if any counts are too small to exit CaraSpace
  #mat=matrix or dataframe containing the data
  #idcol=column number of the subject ID
  #varcol=column number of the categorical variable you want to summarize
  #doplot=T if T, produces plots; change to F to not do plots
  #myrot=F if F, don't rotate the x-axis labels; if T, rotate the x-axis labels
  #myuni=T if T, provide the number of subjects for the data; if F don't provide any info on the number of subjects
  #check5=T if T, check if any cell counts are less than 5; if F, doesn't do the check
  #print5=F if F, don't print any cell counts that are less than 5; if T, print all cell counts
  #usecolours is a vector of user-supplied colours to use for barcharts (ie, "red" or hex codes)

  BCthresh=5 #threshold for checking what is too small to leave CaraSpace

  check5=F
  print5=T

  #A little header and some basic info about data
  cat("\n---------------Variable name: ", dimnames(mat)[[2]][varcol], " column=", varcol, "\n")
  cat("Number of non-missing data points: ",
      length(mat[is.na(mat[, varcol]) == FALSE & mat[, varcol] != "NA", varcol]), "\n")
  if(myuni==T){
    cat("Number of subjects for non-missing:",
        length(unique(mat[is.na(mat[, varcol]) == F & mat[, varcol] != "NA", idcol])), "\n")}

  #check if there is a small number in a count that we need to worry about having it leave CaraSpace
  nomistable=table(mat[, varcol])
  #print(nomistable)
  #print(min(nomistable))
  less5=0
  if(check5==T){
    if(min(nomistable)<BCthresh) {
      less5=1   #flag that there is a small number
      cat("\n\n********* WARNING: A count is less than",BCthresh,"and needs not to leave CaraSpace.\n\n")
    }
  }

  if(less5==0 || print5==T){
    cat("Raw counts:\n") # print out the frequency table and some percents
    print(table(mat[, varcol],useNA = "ifany"))
    cat("As a percentage of non-missing:\n")
    print(100 * round(table(mat[, varcol])/(sum(table(mat[, varcol]))),digits=3))
    cat("\n")
    nlev=length(unique(mat[, varcol]))
    mylas=1;myfactor=1;maxy=1.1*nrow(mat); if (nlev>5) {mylas=2;myfactor=0.75;}
    if(doplot==T){ # If there is a plot to be done
      colors <- rainbow(length(unique(mat[,varcol])))
      if(is.null(usecolours)==F){
        if(length(colours)==length(unique(mat[,varcol]))){
          colors=usecolours
        }else{
          cat("\n\nNOTE: usecolours supplied had length",length(usecolours),"but the number needed is",length(unique(mat[,varcol])),
              "so rainbow colours used instead.")
        }  }

      oldpar=par()$mai
      myfreq=table(mat[,varcol])
      t1=max(myfreq)/maxy
      myi=.05*maxy
      if (max(myfreq)<.5*maxy) {maxy=1.1*t1*maxy;myi=.25*myi}
      labs <- names(table(mat[,varcol]))
      if(myrot==T){
        linch <-  max(strwidth(labs, "inch")+0.4, na.rm = TRUE)
        par(mai=c(.5*linch,.82,0.82,0.42))
      }
      bp=barplot(myfreq,ylab="Frequency",xlab="",main="",ylim=c(0,maxy),
                 col=colors,axes=F,xaxt="n")
      axis(1,at=bp,labels=F)
      axis(2)
      text(bp, myfreq+myi,myfreq,xpd=NA)
      if(myrot==T){ #to allow for rotated tick labels to make things fit
        text(cex=myfactor, x=bp+bp[1], y=-.04*maxy, labs, xpd=TRUE, srt=45,pos=2)
        #print(bp)
        par(mai=oldpar)
      }
      else{axis(1,bp,labs,cex=myfactor);
        mtext(dimnames(mat)[[2]][varcol],side=1,line=2)
      }
    }
  }
  else{
    cat("\n\nNOTE: Nothing printed because there was a number <",BCthresh,"and print5=F says not to print out. Can change print5=T to see the full summary.\n\n ")
  }
}
