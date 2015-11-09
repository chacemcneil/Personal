# Create spectogram
 library(seewave)
 library(tuneR)
 library(audio)
 
 setwd("C:/Users/cmcneil/Documents/projects/Miscellaneous/Speech")
 
 
 data(sheep)
 spectro(sheep)
 
 
 
 front <- readWave("Front.wav")
 spectro(front@left,44100,flim=c(0,5))
 play(front*5)
 plot(front)
 #ends <- round(locator(8)$x*front@samp.rate)
 ends <- c(73413,89243,103489,114569,135675,148866,168389,186329)
 tab <- cbind(calcmfcc(front@left[ends[1]:ends[2]],44100),
              calcmfcc(front@left[ends[3]:ends[4]],44100),
              calcmfcc(front@left[ends[5]:ends[6]],44100),
              calcmfcc(front@left[ends[7]:ends[8]],44100) )
 image(log(tab))
 plot((front[ends[5]:ends[6]]))
 listen(Wave(front@left[ends[1]:ends[2]],samp.rate=44100,bit=16)) # ē
 listen(Wave(front@left[ends[3]:ends[4]],samp.rate=44100,bit=16)) # i
 listen(Wave(front@left[ends[5]:ends[6]],samp.rate=44100,bit=16)) # e
 listen(Wave(front@left[ends[7]:ends[8]],samp.rate=44100,bit=16)) # a
 listen(Wave( altermfcc(front@left[ends[3]:ends[4]],44100,calcmfcc(front@left[ends[1]:ends[2]],44100)) ))
 listen(Wave( altermfcc(front@left[ends[3]:ends[4]],44100,calcmfcc(front@left[ends[3]:ends[4]],44100)) ))
 listen(Wave( altermfcc(front@left[ends[3]:ends[4]],44100,calcmfcc(front@left[ends[5]:ends[6]],44100)) ))
 listen(Wave( altermfcc(front@left[ends[3]:ends[4]],44100,calcmfcc(front@left[ends[7]:ends[8]],44100)) ))
 spectro(Wave(front@left[ends[1]:ends[2]],samp.rate=44100,bit=16),flim=c(0,5))
 spectro(Wave(front@left[ends[3]:ends[4]],samp.rate=44100,bit=16),flim=c(0,5))
 spectro(Wave(front@left[ends[5]:ends[6]],samp.rate=44100,bit=16),flim=c(0,5))
 spectro(Wave(front@left[ends[7]:ends[8]],samp.rate=44100,bit=16),flim=c(0,5))
 
 listen(Wave( altermfcc(sheep@left,8000,calcmfcc(front@left[ends[3]:ends[4]],44100)) ,samp.rate=8000))
 listen(Wave( altermfcc(front@left[ends[5]:ends[6]],44100,calcmfcc(sheep@left,8000)) ,samp.rate=44100,bit=16))
 
 
 
 
 # audio package for recording sounds
 fs <- 44100
 x <- rep(NA,fs*3) #3 seconds
 record(x,fs,2)
 sum(x , na.rm=T)
 play(x)
 
 
 
# End script
 