#' @title Trajectories comparison
#' @description The function is useful to compare the means between set of groups
#' @param data is the data base
#' @param axis are the groups or levels which are defined to compare across the trajectory
#' @param nref is the column or the set of columns of reference which you are interested to compare
#' @param ncomp is the column or set of clumns used to compare "ref" 
#' @details This function shows a plot with the trajectory comparison between "ref" and "comp" arguments
#' @examples
#' month=1:12 ; A=trunc(rnorm(12,20,2)) ; B=trunc(rnorm(12,20,10)) ; C=trunc(rnorm(12,20,12))
#' D=trunc(rnorm(12,20,15)) ; df=data.frame(month,A,B,C,D)
#' example 1
#' CompDB(data=df,axis="month",nref="A",ncomp=c("B","C"))
#' example 2
#' CompDB(data=df,axis="month",nref=c("A","B"),ncomp=c("C","D"))
#' @export
CompDB=function(data,axis,nref,ncomp){
  ID=data[,axis] ; ref=data[,c(axis,nref)] ; comp=data[,c(axis,ncomp)] ; all=data[,c(nref,ncomp)]
  LEN1=length(nref) ; LEN2=length(ncomp) ; bottom=range(all)[1] ; top=range(all)[2]
  dis=trunc(mean(apply(all,2,sd))) ; number=length(unique(data[,axis]))
  if( LEN1==1 && LEN2>=2 ){
     comp$Mean=apply(comp[,ncomp],1,mean)
     comp$Min=apply(comp[,ncomp],1,min)
     comp$Max=apply(comp[,ncomp],1,max)
     plot(-1,-1, ylim=c(bottom-dis,top+dis),xlim=c(1,number) ,type="l",pch=2,col="blue",
          main="Example1",ylab="Mean",xlab="month")
          axis(1, at = c(1:number), cex.axis=1)
       lines(data[,axis],comp[,"Mean"],type="l",pch=2,col="blue",lty=1)
       lines(data[,axis],comp[,"Min"],type="l",pch=2,col="blue",lty=2)
       lines(data[,axis],comp[,"Max"],type="l",pch=2,col="blue",lty=2)
       lines(data[,axis],data[,nref],type="l",pch=2,col="red",lty=1)
       legend("topright",legend=c("ref","comp"),col=c("red","blue"),lwd=3)
   }
  if( LEN1>=2 && LEN2>=2 ){
      comp$Mean=apply(comp[,ncomp],1,mean)
      comp$Min=apply(comp[,ncomp],1,min)
      comp$Max=apply(comp[,ncomp],1,max)
      ref$Mean=apply(ref[,nref],1,mean)
      ref$Min=apply(ref[,nref],1,min)
      ref$Max=apply(ref[,nref],1,max)
     plot(-1,-1, ylim=c(bottom-dis,top+dis),xlim=c(1,number) ,type="l",pch=2,col="blue",
          main="Example1",ylab="Mean",xlab="month")
          axis(1, at = c(1:number), cex.axis=1)
       lines(data[,axis],comp[,"Mean"],type="l",pch=2,col="blue",lty=1)
       lines(data[,axis],comp[,"Min"],type="l",pch=2,col="blue",lty=2)
       lines(data[,axis],comp[,"Max"],type="l",pch=2,col="blue",lty=2)
       lines(data[,axis],ref[,"Mean"],type="l",pch=2,col="red",lty=1)
       lines(data[,axis],ref[,"Min"],type="l",pch=2,col="red",lty=2)
       lines(data[,axis],ref[,"Max"],type="l",pch=2,col="red",lty=2)
       legend("topright",legend=c("ref","comp"),col=c("red","blue"),lwd=3)
   }
  if( LEN1==1 && LEN2==1 ){
     plot(-1,-1, ylim=c(bottom-dis,top+dis),xlim=c(1,number) ,type="l",pch=2,col="blue",
          main="Example1",ylab="Mean",xlab="month")
          axis(1, at = c(1:number), cex.axis=1)
       lines(data[,axis],comp[,ncomp],type="l",pch=2,col="blue",lty=1)
       lines(data[,axis],data[,nref],type="l",pch=2,col="red",lty=1)
       legend("topright",legend=c("ref","comp"),col=c("red","blue"),lwd=3)
   }
}