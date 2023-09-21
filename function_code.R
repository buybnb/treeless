
ff=function(x){
  (2^(x-2)-1)/(2^(x-1)-1)
}

###This is original code to calculate p
math=function(data,kernal="No",sigma=1){
  u=lapply(data,class)
  k=length(u)
  k1=NA
  for(i in 1:k){
    
    k1=c(k1,unlist(u[i]))
  }
  k1=k1[-1]
  
  data1=data[,k1%in%c("numeric","integer")]
  data2=data[,!k1%in%c("numeric","integer")]
  m1=matrix(0,nrow(data),nrow(data))
  m2=matrix(0,nrow(data),nrow(data))
  
  
  if(sum(k1%in%c("numeric","integer"))!=0){
    
    if(kernal=="Yes"){
      mm=apply(data1,2,mean)
      mm=t(matrix(mm,ncol(data1),nrow(data1)))
      data1=exp(-sigma*(data1-mm)^2)
    }
    
    r=apply(data1,2,range)
    r1=r[2,]-r[1,]
    nn=sum(r1==0)
    data1=data1[,r1!=0]
    r1=r1[r1!=0]
    
    for(i in 1:(nrow(data1)-1)){
      m1[i,(i+1):nrow(data1)]=rowSums(1-abs(data.frame(lapply(data1[i,],rep,nrow(data1)-i))-data1[(i+1):nrow(data1),])/data.frame(lapply(r1,rep,nrow(data1)-i)))
    }
    m1[lower.tri(m1)] <- t(m1)[lower.tri(m1)]
    diag(m1)=ncol(data1)
  }
  
  if(sum(!k1%in%c("numeric","integer"))!=0){
    
    w=sapply(data2,function(x) length(unique(x)))
    w1=ff(w)
    for(i in 1:(nrow(data2)-1)){
      w2=data.frame(lapply(data.frame(t(w1)),rep,nrow(data2)-i))
      w2[data.frame(lapply(data2[i,],rep,nrow(data2)-i))==data2[(i+1):nrow(data2),]]=1
      m2[i,(i+1):nrow(data2)]=rowSums(w2)
    }
    m2[lower.tri(m2)] <- t(m2)[lower.tri(m2)]
    diag(m2)=ncol(data2)
    
  }
  
  m=(m1+m2)/(ncol(data)-nn)
  result=list("prox"=m)
}


###This is the code for equation 3
prox2=function(data,d){
  pp=math(data)$prox
  aa=pp*(1-pp^d)/(1-pp)
  aa[is.nan(aa)]=1
  aa=aa/max(aa)
  diag(aa)=1
  aa
}

####q(1-q^D)/(1-q)
#data(iris)
#pp=math(iris[,1:4])$prox

#uu=prox2(pp,d)


###Third one
#apply(pp,2,mean)
###This is the code for equation 4
prox3=function(data,d){
  pp=math(data)$prox
  ppp=pp
  for(i in 1:ncol(pp)){
    ppp[,i]=apply(pp,2,mean)
  }
  
  aa=pp
  for(j in 2:d){
    aa=aa+pp*((1-pp)^(j-1))*(ppp^(j-1))
  }
  
  aa
  
}





