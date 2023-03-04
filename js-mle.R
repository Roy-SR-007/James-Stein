
rm(list=ls())

set.seed(69)

routine = function(n,p,mu_vec,sigma_mat)
{
  X.sample = mvrnorm(n,mu_vec,sigma_mat)
  s = rep(0,p)
  for(i in 1:n)
  {
    s=s+X.sample[i,]
  }
  Xn.bar = s/n # mle of mu
  Xn.bar = as.matrix(Xn.bar)
  
  # james-stein estimator of mu
  h_S = as.numeric(1-((p-2)/(n*(t(Xn.bar)%*%solve(sigma_mat)%*%Xn.bar))))
  mu_js = h_S*Xn.bar
  
  # h(s)+ version of james-stein
  #h_S_plus = ifelse(h_S>0,h_S,0)
  #mu_js_plus = h_S_plus*Xn.bar
  
  res = matrix(0,ncol=2,nrow=p)
  res[,1]=Xn.bar
  res[,2]=mu_js
  #res[,3]=mu_js_plus
  
  return(res)
  
}

m = c(1,1,1)
S = diag(1,ncol=3,nrow=3)
nsim = 30
res_list = list(0) # stores the results as list

for(i in 1:nsim)
{
  res_list[[i]] = routine(10,3,m,S)
}

# plotting james-stein and mle

x_js = array(0)
x_mle = array(0)
y_js = array(0)
y_mle = array(0)
z_js = array(0)
z_mle = array(0)

for(i in 1:nsim)
{
  x_js[i] = res_list[[i]][1,2]
  y_js[i] = res_list[[i]][2,2]
  z_js[i] = res_list[[i]][3,2]
  
  x_mle[i] = res_list[[i]][1,1]
  y_mle[i] = res_list[[i]][2,1]
  z_mle[i] = res_list[[i]][3,1]
  
  
}

x = rep(0,nsim)
y = rep(0,nsim)
z = rep(0,nsim)

xx = rep(1,nsim)
yy = rep(1,nsim)
zz = rep(1,nsim)

#js_fig=scatterplot3d(x_js,y_js,z_js,pch=19,color="blue")
#js_fig$points3d(x_mle,y_mle,z_mle,pch=16,col="lightblue",cex=0.2)
#js_fig$points3d(x,y,z,pch=19)

type = as.factor(c(rep("js",nsim),rep("mle",nsim),rep("zero",nsim),rep("one",nsim)))
data = data.frame(x=c(x_js,x_mle,x,xx),y=c(y_js,y_mle,y,yy),z=c(z_js,z_mle,z,zz),type=type)


js_fig=scatter3d(data$x,data$y,data$z,surface=TRUE,groups=data$type,ellipsoid=TRUE,
                 surface.col=c("yellow","blue","green","red"),bg.col="black",xlab="X",ylab="Y",zlab="Z")

