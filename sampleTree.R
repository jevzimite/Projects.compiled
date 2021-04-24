# index = sample(1:nrow(iris), floor(nrow(iris)/2))
# train = iris[index,]
# test = iris[-index,]
# 
# nnfun = function(i){
# nn = neuralnet::neuralnet(Species ~., data = train, hidden = c(4,4), linear.output = 0, act.fct = "tanh")
# # plot(nn)
# 
# pred = round(predict(nn, test),3)
# colnames(pred)= c(levels(iris$Species))
# s = ifelse(pred[,1]>pred[,2] & pred[,1]>pred[,3] , 1, 0)
# vc = ifelse(pred[,2]>=pred[,1] & pred[,2]>pred[,3] , 2, 0)
# vg = ifelse(pred[,3]>pred[,1] & pred[,3]>pred[,2] , 3, 0)
# res = cbind(s, vc, vg)
# res = rowSums(res)
# res[res == 1] = "setosa"
# res[res == 2] = "versicolor"
# res[res == 3] = "virginica"
# res = (res == test$Species)
# k = c(res[res=="TRUE"])
# return(length(k)/150)
# }
# nnlist = replicate(n = 10, {
# sapply(1:2,nnfun)})
# matplot(nnlist, type= "l", lty = "solid", col = rethinking::col.alpha("red", .35), lwd = .6)




#### forest ####
par(mfrow=c(2,2))
finalFUN = function(j){
fun = function(i){
  a = randomForest::randomForest(Species ~ ., train, mtry = j, ntree = i)
  mean(a$err.rate)}
x = c(990:1010)
error = replicate(n = 25,{
  a = c(sapply(x, fun))
  a = round(a,4)*100
  return(a)})
matplot(error, type = "l", 
        lty = "solid", 
        lwd = .25, 
        ylim = c(0,20), 
        col = rethinking::col.alpha(rainbow(1), .5),
        )
lines(y = c(mean(error), mean(error)), x = c(-5,30), lwd = .8, lty = "dashed")
text(x = 10, y = 15, labels = round(mean(error),3))
}
sapply(1:4, finalFUN)
#####

rf = randomForest::randomForest(Species~., test, mtry = 3, ntree = 1000)

reprtree:::plot.getTree(rf)
