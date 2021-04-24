sides = 250
frames = 20

dist = rep(0, sides**2)
mat = matrix(dist, ncol = sides)

# mat = scales::rescale(ambient::noise_perlin(c(sides,sides)))

storage = array(rep(0, (10**2)*frames), c(sides,sides,frames))

for(o in 1:frames){
  mat = mat + matrix(scales::rescale(ambient::noise_perlin(c(sides,sides)))*.0094,
                     ncol = sides)
  a = c()
  for (i in 1:sides**2){
    a[i] = mat[i]}
  # a = scales::rescale(a/3) + 1
  mat = matrix(a, ncol = sides)
  storage[,,o] = mat
}

storage = storage*.4

storage2 = storage[,,frames:1]

storage = array(
  c(c(storage), c(storage2)), dim = c(dim(mat), frames*2)
)

# for (i in 1:frames)
#   storage[,,i] = scales::rescale(storage[,,i])

caTools::write.gif(storage, filename = 'xxx.gif', col= 'jet', delay=0, )