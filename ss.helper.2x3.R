a1 = c(9,9,5,9)
a2 = c(8,9,12,11)
a3 = c(13,18,15,14)
b1 = c(7,6,9,10)
b2 = c(2,4,3,7)
b3 = c(2,6,2,2)

pool = c(a1,a2,a3,b1,b2,b3)

ssT.2x3 = function(a1,a2,a3,b1,b2,b3){
  pool = c(a1,a2,a3,b1,b2,b3)
  ssT = sum((pool-mean(pool))**2)
  return(ssT)
}

ssE.2x3 = function(a1,a2,a3,b1,b2,b3){
  ssE = sum((a1 - mean(a1))**2,
  (a2 - mean(a2))**2,
  (a3 - mean(a3))**2,
  (b1 - mean(b1))**2,
  (b2 - mean(b2))**2,
  (b3 - mean(b3))**2)
  return(ssE)
}

ssCells.2x3 = function(a1,a2,a3,b1,b2,b3){
  ssC = sum(
  length(a1)*(mean(a1)-mean(pool))**2,
  length(a2)*(mean(a2)-mean(pool))**2,
  length(a3)*(mean(a3)-mean(pool))**2,
  length(b1)*(mean(b1)-mean(pool))**2,
  length(b2)*(mean(b2)-mean(pool))**2,
  length(b3)*(mean(b3)-mean(pool))**2)
  return(ssC)
}

ssR.2x3 = function(a1,a2,a3,b1,b2,b3){
  (length(c(a1,a2,a3))) * (mean(c(a1,a2,a3)) - mean(pool))**2 +
  (length(c(b1,b2,b3))) * (mean(c(b1,b2,b3)) - mean(pool))**2 
}

ssCol.2x3 = function(a1,a2,a3,b1,b2,b3){
  (length(c(a1,b1))) * (mean(c(a1,b1)) - mean(pool))**2 +
  (length(c(a2,b2))) * (mean(c(a2,b2)) - mean(pool))**2 +
  (length(c(a3,b3))) * (mean(c(a3,b3)) - mean(pool))**2 
}

ssI.2x3 = ssCells.2x3(a1,a2,a3,b1,b2,b3) - ssR.2x3(a1,a2,a3,b1,b2,b3) - ssCol.2x3(a1,a2,a3,b1,b2,b3) 
ssI.2x3 = ssT.2x3(a1,a2,a3,b1,b2,b3) - ssE.2x3(a1,a2,a3,b1,b2,b3) - ssR.2x3(a1,a2,a3,b1,b2,b3) - ssCol.2x3(a1,a2,a3,b1,b2,b3)

ssCells.2x3(a1,a2,a3,b1,b2,b3)
  ssR.2x3(a1,a2,a3,b1,b2,b3)
  ssCol.2x3(a1,a2,a3,b1,b2,b3)
  ssI.2x3
ssE.2x3(a1,a2,a3,b1,b2,b3)
ssT.2x3(a1,a2,a3,b1,b2,b3)