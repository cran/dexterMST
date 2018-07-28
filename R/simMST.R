###################
# Simulate MST data using a Rasch model
#
# booklet
# For example booklets[[1]]$modules=c(1,2,4)
#             booklets[[1]]$min_scores=c(0,0,0)
#             booklets[[1]]$max_scores=c(5,5,10)
# modules is a vector of list modules=vector(mode='list', length=7)
# For example modules[[1]]$items=..
#             modules[[1]]$max_score

sim_MST<-function(booklets, modules, theta, delta, alpha, routing=c("all", "last"))
{
  routing=match.arg(routing)
  nP=length(theta)
  nB=length(booklets)
  dat=matrix(0,nP,length(delta))
  bk=rep(0,nP)
  for (i in 1:length(delta)) dat[,i]=1*(rlogis(nP,0,1)<=(alpha[i]*theta-delta[i]))
  scores=matrix(NA,nP,length(modules))
  for (m in 1:length(modules)) 
  {
    scores[,m]=apply(dat,1,function(x)sum(alpha[modules[[m]]$items]*x[modules[[m]]$items]))
  }
  first=1:length(delta)
  last=first
  
  exclude=vector("list", nB)
  its=vector("list",nB)
  for (i in 1:nB)
  {
    booklets[[i]]$routing=routing
    if (booklets[[i]]$routing=="last")
    {
      exclude[[i]]=rep(0,nP)
      for (j in 1:length(booklets[[i]]$modules))
      {
        its[[i]]=c(its[[i]],modules[[booklets[[i]]$modules[j]]]$items)
        disc=alpha[modules[[booklets[[i]]$modules[j]]]$items]
        rsums=apply(dat[,modules[[booklets[[i]]$modules[j]]]$items], 1, function(x)disc%*%x)
        exclude[[i]]=exclude[[i]]+1*(rsums<booklets[[i]]$min_scores[j])
        exclude[[i]]=exclude[[i]]+1*(rsums>booklets[[i]]$max_scores[j])
      }
      bk[exclude[[i]]==0]=i
    }
    if (booklets[[i]]$routing=="all")
    {
      exclude[[i]]=rep(0,nP)
      cum_scores=matrix(0,nP)
      for (j in 1:length(booklets[[i]]$modules))
      {
        cum_scores=cum_scores+scores[,booklets[[i]]$modules[j]]  ### Here's the difference between last and all
        its[[i]]=c(its[[i]],modules[[booklets[[i]]$modules[j]]]$items)
        exclude[[i]]=exclude[[i]]+(cum_scores<booklets[[i]]$min_scores[j])
        exclude[[i]]=exclude[[i]]+(cum_scores>booklets[[i]]$max_scores[j])
      }
      bk[exclude[[i]]==0]=i
    }
  }
  
  compl_dat=dat
  for (i in 1:nB)
  {
    dat[exclude[[i]]==0,(1:length(delta))[-its[[i]]]]=NA
    booklets[[i]]$item_score=sum(alpha[its[[i]]])
    booklets[[i]]$items=its[[i]]
    ms=sum(booklets[[i]]$item_score)
    booklets[[i]]$scoretab=vector("numeric",ms+1)
    ff=rowSums(scores[bk==i,booklets[[i]]$modules])
    for (s in 0:ms) booklets[[i]]$scoretab[s+1]=sum(ff==s)
    ## first and last per module now
    booklets[[i]]$first= vector(mode='list', length=length(booklets[[i]]$modules))
    booklets[[i]]$last = vector(mode='list', length=length(booklets[[i]]$modules))
    for (j in 1:length(booklets[[i]]$modules))
    {
      booklets[[i]]$first[[j]]= first[modules[[booklets[[i]]$modules[[j]]]]$items]
      booklets[[i]]$last[[j]] = last[modules[[booklets[[i]]$modules[[j]]]]$items]
    }
  }
  sufI=array(0,ncol(dat))
  for (i in 1:ncol(dat)) {sufI[i]=sum(dat[,i]==1, na.rm=T)}
 
  o=order(bk)
  out=list(compl_dat=compl_dat[o,],obs.dat=dat[o,],sufI=sufI,scores=scores[o,],bk=bk[o],theta=theta[o],
           booklets=booklets)
  return(out)
}