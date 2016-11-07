# Create spectogram
 library(seewave)
 library(audio)
 library(tuneR)
 
 setwd("C:/Users/cmcneil/Documents/projects/Miscellaneous/Speech")
 setwd("projects/Miscellaneous/soundfiles")
 
 source('~/Projects/Miscellaneous/Personal/General Code/UsefulFunctions.R')
 
 data(sheep)
 spectro(sheep)
 
 
 
 front <- readWave("Front.wav")
 mysamp <- readWave("Back.wav")
 spectro(front@left,44100,flim=c(0,5))
 p <- audio::play(front*5)
 pause(p)
 listen(sheep)
 plot(sheep)
 #ends <- round(locator(8)$x*front@samp.rate)
 ends <- c(73413, 89243, 103489,  114569, 135675, 148866, 168389, 186329)
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
 this <- record(x,fs,1)
 sum(this , na.rm=T)
 play(x)
 
 
 
 play(sin((1:1000)/100))
 
 tryrec <- function() {
   require(audio)
   where <- rep(NA_real_, 16e3)
   rate <- 8000
   channels <- 1
   
   a <- .Call("audio_recorder", where, as.double(rate), as.integer(channels), 
              PACKAGE = "audio")
   .Call("audio_start", a, PACKAGE = "audio")
   invisible(a)
   where
 }
 
 
 
# End script
 