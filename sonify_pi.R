library(tidyverse)
library(tuneR)
library(seewave)

#test
t = seq(0, 3, 1/8000) #times in seconds if sample for 3 seconds at 8000Hz
u = (2^15-1)*sin(2*pi*440*t) #440 Hz sine wave that lasts t length seconds (here, 3 seconds)
w = Wave(u, samp.rate = 8000, bit=16) #make the wave variable
play(w) #play the wave data by the default player 

f <- 8000 # sampling frequency
d <- 3    # duration (1 s)
cf <- 460 # carrier frequecy (440 Hz, i.e. flat A tone)
# pure sinusoidal tone
s <- synth(f=f,d=d,cf=cf, listen = TRUE)
# pure triangular tone
s <- synth(f=f,d=d,cf=cf, signal="saw", listen = TRUE)
# pure tone with triangle overall shape
s <- synth(f=f,d=d,cf=cf,shape="decr", listen = TRUE, output = "Wave") ## good
fadew(s, f=4000, din = 4, shape = "", listen = TRUE)
# pure tones with am
s <- synth(f=f,d=d,cf=cf,am=c(50,10), listen = TRUE)
# pure tones with am
# and phase shift of pi radian (180 degrees)
s <- synth(f=f,d=d,cf=cf,am=c(50,10,pi), listen = TRUE)
# pure tone with +1000 Hz linear fm 
s <- synth(f=f,d=d,cf=cf,fm=c(0,0,1000,0,0), listen = TRUE) #interesting
# pure tone with sinusoidal fm
# (maximum excursion of 250 Hz, frequency of 10 Hz)
s <- synth(f=f,d=d,cf=cf,fm=c(250,10,0,0,0), listen = TRUE)
# pure tone with sinusoidal fm
# (maximum excursion of 250 Hz, frequency of 10 Hz,
# phase shift of pi radian (180 degrees))
s <- synth(f=f,d=d,cf=cf,fm=c(250,10,0, pi,0), listen = TRUE)
# pure tone with sinusoidal am
# (maximum excursion of 250 Hz, frequency of 10 Hz)
# and linear fm (maximum excursion of 500 Hz)
s <- synth(f=f,d=d,cf=cf,fm=c(250,10,500,0,0), listen = TRUE)
# the same with am
s <- synth(f=f,d=d,cf=cf,am=c(50,10), fm=c(250,10,250,0,0), listen = TRUE)
# the same with am and a triangular overall shape 
s <- synth(f=f,d=d,cf=cf,shape="tria",am=c(50,10), fm=c(250,10,250,0,0), listen = TRUE)
# an harmonic sound
s <- synth(f=f,d=d,cf=cf, harmonics=c(1, 0.5, 0.25), listen = TRUE)
# a clarinet-like sound
clarinet <- c(1, 0, 0.5, 0, 0.14, 0, 0.5, 0, 0.12, 0, 0.17)
s <- synth(f=f, d=d, cf = 235.5, harmonics=clarinet, listen = TRUE)
# inharmonic FM sound built 'manually'
fm <- c(250,5,0,0,0)
F1<-synth(f=f,d=d,cf=cf,fm=fm)
F2<-synth(f=f,d=d,a=0.8,cf=cf*2,fm=fm)
F3<-synth(f=f,d=d,a=0.6,cf=cf*3.5,fm=fm)
F4<-synth(f=f,d=d,a=0.4,cf=cf*6,fm=fm)
final1<-F1+F2+F3+F4
spectro(final1,f=f,wl=512,ovlp=75,scale=FALSE)


a<-noisew(d=5,f=4000)
op<-par(mfrow=c(3,1))
fadew(a,f=4000,din=1,dout=2,plot=TRUE,title="Linear",cexlab=0.8, listen = TRUE)
fadew(a,f=4000,din=1,dout=2,shape="exp",plot=TRUE,title="Exponential shape",
      colwave="blue",coltitle="blue",cexlab=0.8, listen = TRUE)
fadew(a,f=4000,din=1,dout=2,shape="cos",plot=TRUE,title="Cosinus-like shape",
      colwave="red",coltitle="red",cexlab=0.8, listen = TRUE)
