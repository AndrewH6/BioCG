#' provides a nice summary and test for a continuous variable by a grouping variable, with optional testing among groups

#' @export

bicontvarsum=function(mat, idcol, varcol, groupcol,myrot=F,mytest=T,myuni=T,check5=F,print5=T,usecolours=NULL){
  #provides a nice summary and test for a continuous variable by a grouping variable, with optional testing among groups
  #assumes that missing data are NA or "NA", all other values would be considered non-missing (ie 9999) so may need to change up some things in pre-processing of any truly missing values
  #Doesn't check to see if any counts are too small to exit CaraSpace because it is continuous
  #mat=matrix or dataframe containing the data
  #idcol=column number of the subject ID
  #varcol=column number of the continuous variable you want to summarize
  #groupcol=column number of the grouping variable
  #doplot=T if T, produces plots; change to F to not do plots
  #myrot=F if F, don't rotate the x-axis labels; if T, rotate the x-axis labels
  #check5=T if T, check if any cell counts are less than 5; if F, doesn't do the check
  #print5=F if F, don't print any cell counts that are less than 5; if T, print all cell counts
  #usecolours is a vector of user-supplied colours to use for barcharts (ie, "red" or hex codes)
  BCthresh=5 #threshold for checking what is too small to leave CaraSpace

  check5=F
  print5=T

  grouplev <- dimnames(table(mat[, groupcol]))[[1]]
  nlev <- length(grouplev)
  for(i in 1:nlev) {
    cat("---------",dimnames(mat)[[2]][groupcol],"=",grouplev[i], "---------\n")
    mat2 <- mat[mat[, groupcol] == grouplev[i],  ]
    contvarsum(mat2, idcol, varcol,doplot=F,myuni=myuni,check5=check5,print5=print5)
  }
  if(mytest==T){
    cat("\n\nTest equality of means:\n")
    cat("NOTE: Tests assume independent data (ie distinct subjects) and wouldn't be valid if multiple data points per subject.\n\n")
    print(anova(lm(as.formula(paste(names(mat)[varcol],"~",names(mat)[groupcol],sep="")),data=mat)))
    print(summary(lm(as.formula(paste(names(mat)[varcol],"~",names(mat)[groupcol],sep="")),data=mat)))
    cat("\n\nNon-parametric test:\n")
    print(kruskal.test(as.formula(paste(names(mat)[varcol],"~ as.factor(",names(mat)[groupcol],")",sep="")), data = mat) )
    cat("\n");
  }
  mylas=1;myfactor=.75; if (nlev>5) {mylas=2;myfactor=0.55}
  miny=min(mat[,varcol],na.rm =T)
  if(abs(miny)<100) miny=0
  boxplot(mat[,varcol]~mat[,groupcol],ylim=c(miny,max(mat[,varcol],na.rm=T)),ylab=dimnames(mat)[[2]][varcol],xlab=dimnames(mat)[[2]][groupcol],main="",las=mylas,
          cex.axis=myfactor)
  if(nlev==2){
    colors <-c("darkblue", "red")
    if(is.null(usecolours)==F){
      if(length(colours)==2){
        colors=usecolours
      }else{
        cat("\n\nNOTE: usecolours supplied had length",length(usecolours),"but the number needed is",length(unique(mytwoframe[,2])),
            "so rainbow colours used instead.")
      }  }

    l=list(mat[mat[,groupcol]==grouplev[1],varcol],mat[mat[,groupcol]==grouplev[2],varcol])
    breaks <- pretty(unlist(l))
    levs <- levels(cut(unlist(l), breaks=breaks,dig.lab=6,include.lowest=T,right=F))
    #check the frequencies
    tthist1=hist(mat[mat[,groupcol]==grouplev[1],varcol],breaks=breaks,plot=F)
    tthist2=hist(mat[mat[,groupcol]==grouplev[2],varcol],breaks=breaks,plot=F)
    binmin=c(tthist1$counts,tthist2$counts)
    binmin=binmin[binmin>0]  #remove all the 0s, since ok to print 0 in a plot because it doesn't identify anyone
    if(print5==T || (min(binmin)>=BCthresh)){
      multhist(l,xlab=dimnames(mat)[[2]][varcol],col=colors, legend=T,breaks=breaks,names.arg = levs)
      legend("topright", c(paste(dimnames(mat)[[2]][groupcol],grouplev[1],sep="="),
                           paste(dimnames(mat)[[2]][groupcol],grouplev[2],sep="=")), fill = colors, bty = "n")
    }
    else{
      cat("\nNOTE: Barchart not printing because cell size smaller than",BCthresh,". Can see by forcing print5=T.")
    }

  }
}
