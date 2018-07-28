

sim_RM<-function(theta,delta)
{
  nP=length(theta)
  dat=matrix(0,nP,length(delta))
  for (i in 1:length(delta)) dat[,i]=1*(rlogis(nP,0,1)<=(theta-delta[i]))
  return(dat)
}
make_sim = function()
{
  ## Simulate data set with all routing
  scoring_rules<-data.frame(item_id=rep(paste0("item",sprintf("%02.0f",1:70)), each=2),
                            response=rep(0:1,times=70),
                            item_score=rep(0:1,times=70))
  
  
  design<-data.frame(item_id=paste0("item",sprintf("%02.0f",1:70)),
                     module_id=rep(c('M4','M2','M5','M1','M6','M3', 'M7'),times=rep(10,7)),
                     item_position=rep(1:10,7))
  
  delta = sort(runif(70,-1,1))
  ## ability
  # normal
  theta = rnorm(2000,0,1)
  #theta = sapply(rbinom(2000,1,1/3),function(x){
  #  if(x==0) return(rnorm(1,-1.5,0.5)) 
  #  else return(rnorm(1,1,1))})
  
  data = data.frame(sim_RM(theta,delta))
  colnames(data) = sprintf("item%02.0f",1:70)
  data$person_id = 1:nrow(data)
  
  
  save(data,theta,delta, design, file = 'tests/data_sim.RData')
}
