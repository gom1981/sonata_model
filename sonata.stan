data {
  int N;
  int M;
  int thefts[N,M];
}
parameters {
  real<lower=0.05> a;
  real<lower=0> b;
  real<lower=1> c;
  real<lower=0> d;
  real<lower=0> r0;
  real<lower=0> x0;
  real<lower=1,upper=100> bump;
  real<lower=0> carseed;
}
transformed parameters {
  matrix[N,M] lam;
  
  for (i in 1:N){
    for(j in 1:M){
      lam[i,j]=0.0;
      if((i-j)<26){
        if(i<36){
          lam[i,j]=pow(i,d)*carseed/(1.0+a*exp(b*pow(((j-i+25.0)/25.0),c)));
          }else{
          if((j>20) && (i<45)){
          lam[i,j]=pow(i,d)*bump*(1/(1+exp(-r0*(j-20-x0))))*carseed/(1.0+a*exp(b*pow(((j-i+25.0)/25.0),c)));
          }else{
          lam[i,j]=pow(i,d)*carseed/(1.0+a*exp(b*pow(((j-i+25.0)/25.0),c)));
          }
        }
      }
    }
  }
}
model {
  a ~ exponential(.1);
  b ~ exponential(.1);
  c ~ exponential(1);
  d ~ exponential(1);
  r0 ~ exponential(1);
  x0 ~ exponential(.1);
  bump ~ exponential(.1);
  carseed ~ exponential(.1);
  for (i in 1:N){
    for(j in 1:M){
      if((i-j)<26){
      thefts[i,j]~poisson(lam[i,j]);
      }
    }
  }
}
