# Speech analytics
 library(seewave)
 library(tuneR)
 
 setwd("C:/Users/cmcneil/Documents/projects/Miscellaneous/Speech")
 
 
 ## Functions
 power <- function(fvec) {
   power <- Mod(fvec)^2/length(fvec)
   power
 }
 # mfvec <- mel(fvec)
 # fvec  <- mel(mfvec,inv=T)
 melbank <- function(pvec, fs = 44100, numbanks = 26, minfreq = 100, maxfreq = 8000, transform = F, mfvec2 = NULL) {
   # 
   mfseq <- seq(mel(minfreq),mel(maxfreq),length.out=numbanks+2)
   fseq  <- mel(mfseq,inverse=T)
   mf <- mfseq[2:(numbanks+1)]
   
   freq <- (1:length(pvec)-1)*fs/length(pvec)
   A <- sapply(1:numbanks,function(i) {
     pmax(0,
          pmin( (freq-fseq[i])/(fseq[i+1]-fseq[i]),
                (fseq[i+2]-freq)/(fseq[i+2]-fseq[i+1]) ) )
   })
   mfvec <- t(pvec%*%A)
   if(!transform)
     return(mfvec)
   B <- A%*%(mfvec2/mfvec)
   B*pvec
 }
 DCT <- function(vec,inverse=F) {
   # Discrete Cosine Transform
   n <- length(vec)
   E <- diag(1,n)
   E <- cos(pi/n*(col(E)-.5)*(row(E)-1))
   if(inverse) {
     E <- t(E)
     E[,1] <- 1/2
     E <- E*2/n
   }
   E%*%vec
 }
 rescale <- function(fvec,pvec) {
   # Scales the frequency vector fvec so that the Modulus matches pvec
   pvec.orig <- power(fvec)
   fvec*(pvec/pvec.orig)
 }
 
 calcmfcc <- function(vec, fs, DCT = T, bin = T, ...) {
   # Find mel frequency cepstral coefficients
   fvec <- fft(vec)
   pvec <- power(fvec)
   if(bin)
     mfbin <- melbank(pvec, fs=fs, ...)
   else
     mfbin <- mel(pvec)
   lmfbin <- log(mfbin)
   if(DCT)
     mfcc <- DCT(lmfbin)
   else
     mfcc <- Re(fft(lmfbin))
   mfcc
 }
 
 altermfcc <- function(vec,fs,mfcc,...) {
   # Alter mel frequency cepstral coefficients
   fvec <- fft(vec)
   pvec <- power(fvec)
   lmfbin2 <- DCT(mfcc,inverse=T)
   mfbin2  <- exp(lmfbin2)
   pvec2 <- melbank(pvec,fs=fs,transform=T,mfvec2=mfbin2)
   fvec2 <- rescale(fvec,pvec2)
   vec2  <- Re(fft(fvec2,inverse=T))/length(fvec2)
   vec2
 }
 altermfcc2 <- function(vec,fs,mfcc,...) {
   # Alter mel frequency cepstral coefficients
   fvec <- fft(vec)
   pvec <- power(fvec)
   lmfbin2 <- DCT(mfcc,inverse=T)
   mfbin2  <- exp(lmfbin2)
   pvec2 <- melbank(pvec,fs=fs,transform=T,mfvec2=mfbin2)
   #pvec2[-1] <- pvec2[-1] + rev(pvec2[-1])
   fvec2 <- rescale(fvec,pvec2)
   vec2  <- Re(fft(fvec2,inverse=T))/length(fvec2)
   vec2
 }
 ## Process
 
 # Read signal
 
 #wav <- readWave("c:/users/cmcneil/Documents/Projects/Miscellaneous/Speech/Sample.wav")
 wav <- readWave("c:/users/cmcneil/Documents/Projects/Miscellaneous/SoundFiles/Sample.wav")
 veclong <- wav@left*100
 vec <- veclong
 vec <- veclong[6e4:1.5e5]
 vec <- veclong[8.1e4:8.3e4]
 
 mfcc <- calcmfcc(vec,44100)
 
 mfcc2 <- mfcc
 mfcc2 <- mfcc2
 mfcc2 <- c(mfcc[1],10,3,-2,0,4,mfcc[7:26])
 mfcc2 <- mfcc[c(1,5:2,9:6,10:26)]*2
 mfcc2 <- mfcc[c(1,2,4,3,5:26)]
 mfcc2[1] <- mfcc[1]
 
 
 
 
 
 
 
 ceps(veclong[6e4:1.5e5], 44100)
 
 
 
 
# End script
 