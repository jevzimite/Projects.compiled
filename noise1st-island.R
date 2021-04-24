# storage = round(ambient::noise_worley(dim = c(100,100,100)),2 )
# storage = round(ambient::noise_perlin(dim = c(100,100,100)),1)
# storage = ambient::noise_worley(dim = c(2000,2000,100))
n = 1
w = 1000


storage = round(ambient::noise_perlin(dim = c(w,w,n), fractal = 'rigid-multi',
                                      frequency = 0.005, octaves = .1, 
                                      pertubation = 'fractal', pertubation_amplitude = 10)
                ,1)

storage2 = storage[,,n:1]
storage3 = c(storage,storage2)
storage = (array(storage3 , c(w,w,n*2))*-1)

red.yellow.green = colorRampPalette(c(3,7,2,1))

cf = colorRampPalette(c('darkblue',"darkblue",'darkblue',5,5,6))



caTools::write.gif(storage, filename = 'xxx.gif', col= cf, delay=0, )

a =as.matrix(storage[,,1])
# a = round(a,0)

cf = colorRampPalette(c('darkblue','darkblue',
                        'chocolate4', 'chocolate4', 
                        'white','white'))
cf = cf(w**2)

plot(as.factor(cf))
levelplot(a, col = cf)

# levelplot(
# mat0)
