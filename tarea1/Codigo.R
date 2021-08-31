dim=300
dur=10000
pos=rep(0,dim)
mayor=0
for (t in 1:dur){
  cual=sample(1:dim,1)
  if(runif(1)<0.5){
    pos[cual] = pos[cual] + 1
  } else {
     pos[cual] = pos[cual] - 1
  }
  mayor=max(mayor, sum(abs(pos)))#manhattan
}
print(mayor)
