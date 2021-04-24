a1 = c(9,9,5,9)
a2 = c(8,9,12,11)

b1 = c(7,6,9,10)
b2 = c(2,4,3,7)


pool = c(a1,a2,b1,b2)

ssT.2x2 = function(a1,a2,b1,b2){
  pool = c(a1,a2,b1,b2)
  ssT = sum((pool-mean(pool))**2)
  return(ssT)
}

ssE.2x2 = function(a1,a2,b1,b2){
  ssE = sum((a1 - mean(a1))**2,
            (a2 - mean(a2))**2,
            (b1 - mean(b1))**2,
            (b2 - mean(b2))**2)
  return(ssE)
}

ssCells.2x2 = function(a1,a2,b1,b2){
  ssC = sum(
    length(a1)*(mean(a1)-mean(pool))**2,
    length(a2)*(mean(a2)-mean(pool))**2,
    length(b1)*(mean(b1)-mean(pool))**2,
    length(b2)*(mean(b2)-mean(pool))**2)
  return(ssC)
}

ssR.2x2 = function(a1,a2,b1,b2){
  (length(c(a1,a2))) * (mean(c(a1,a2)) - mean(pool))**2 +
    (length(c(b1,b2))) * (mean(c(b1,b2)) - mean(pool))**2 
}

ssCol.2x2 = function(a1,a2,b1,b2){
  (length(c(a1,b1))) * (mean(c(a1,b1)) - mean(pool))**2 +
    (length(c(a2,b2))) * (mean(c(a2,b2)) - mean(pool))**2 
}

ssI.2x2 = ssCells.2x2(a1,a2,b1,b2) - ssR.2x2(a1,a2,b1,b2) - ssCol.2x2(a1,a2,b1,b2)
ssI.2x2 = ssT.2x2(a1,a2,b1,b2) - ssE.2x2(a1,a2,b1,b2) - ssR.2x2(a1,a2,b1,b2)- ssCol.2x2(a1,a2,b1,b2)


ssCells.2x2(a1,a2,b1,b2)
  ssR.2x2(a1,a2,b1,b2)
  ssCol.2x2(a1,a2,b1,b2)
  ssI.2x2
ssE.2x2(a1,a2,b1,b2)
ssT.2x2(a1,a2,b1,b2)