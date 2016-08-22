library(deldir)

dd <- deldir(x=rnorm(10),    # x-coordinates
             y=rnorm(10),    # y-coordinates
             rw=c(-4,4,-2,2) # Outer boundaries
             )

plot(dd)

names(dd)
