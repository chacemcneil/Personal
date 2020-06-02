# Experiment with princomp
 library(data.table)
 
 # Two functions exist: princomp and prcomp
 # princomp uses eigen, prcomp uses svd
 # princomp estimates variances as sig^2_hat, prcomp uses s^2
 
 n <- 1e3
 p <- 10
 sigs <- rgamma(p, 3, 1/12)
 mus  <- rnorm(p, 20, 7)
 dt <- data.table(do.call(cbind, lapply(1:p, function(i) rnorm(n, mus[i], sigs[i]))))
 setnames(dt, letters[1:p])
 
 sapply(dt, sd)
 princomp(X)
 X <- data.matrix(dt)
 
 n <- nrow(X)
 
 Xc <- scale(X, scale = F)
 CovX <- cov(Xc)
 egn <- eigen(CovX)
 egn
 sqrt(egn$values)
 apply(egn$vectors, 2, function(x) sum(x^2))
 
 
 
 pc1 <- princomp(dt, cor = F, scores = T)
 names(pc1)
 pc1$sdev
 apply(pc1$loadings, 2, function(x) sum(x^2)) # Eigenvectors are unit vectors
 
 pc2 <- prcomp(X) # includes options: cor = F, center = T, scale = F
 names(pc2)
 pc2$sdev
 apply(pc2$rotation, 2, function(x) sum(x^2)) # Eigenvectors are unit vectors
 
 # Eigenvalues:
 egn$values
 pc1$sdev^2 # off because of variance calculation
 pc1$sdev^2*n/(n-1) # corrected
 pc2$sdev^2
 
 # Loadings are defined as eigenvectors * sqrt(eigenvalues), both pc1$loadings and pc2$rotation are just the eigenvectors !
 
 # Eigenvectors (notice signs are potentially reversed between pc1 an pc2):
 egn$vectors
 pc1$loadings[]  # no correction needed on eigenvectors
 pc2$rotation
 
 # Loadings:
 egn$vectors %*% diag(sqrt(egn$values))
 pc1$loadings %*% diag(pc1$sdev) # again slightly different
 pc2$rotation %*% diag(pc2$sdev)
 
 
 # Scores:
 head(pc1$scores)
 head(scale(X, scale = F) %*% pc1$loadings)
 head(scale(X, scale = F) %*% pc2$rotation)
 head(pc2$x)
 
 
 
 
# End script
 