

# guide = seq(from = (min(df)-1),to =(max(df)+1), length.out =  nrow(df))
# df = data.frame(df,guide)
x = round(seq(from=1, to=60, length.out = 5),0)
fun = function(i){
nC = i
############################################################
par(mar = rep(0, 4))
cols <- colorRampPalette(c("darkred",7,"darkgreen"))(i)
# cols <- colorRampPalette(c("darkgreen",7,"darkred"))(i)
for(i in 1:ncol(df))
  df[,i]=as.numeric(df[,i])
for(i in 1:ncol(df))
  df[,i]=scale(df[,i])
MAT = t(as.matrix(apply(df, 2, rev)))

# image( (MAT), axes = FALSE, 
#        col = rainbow(nC))
image( (MAT), axes = FALSE, 
       col = cols)
par(mfrow=c(1,1))
}
# sapply(x, fun)
fun(20)

head(df)

