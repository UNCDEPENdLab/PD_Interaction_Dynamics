load("H:\\align_split_atdifferentfreqs.RData")

#CCF Participants
pdf("allccf.pdf", width=11, height=8.5)
cross <- lapply(align_split_4Hz, function(x) {
  ccf(x$l_ibi_interp_detrend, x$r_ibi_interp_detrend, main = x$PTNUM[[1]], lag.max=200)
  })
dev.off()

#ACF of Left Participants
interL <-lapply(align_split_4Hz, function(x) {acf(x$l_ibi_interp_detrend, main = x$PTNUM[[1]], lag.max = 200)})
dev.off()


#ACF of Right Participants
pdf("allAcfR.pdf", width = 11, height = 8.5)
interR <-lapply(align_split_4Hz, function(x) {acf(x$r_ibi_interp_detrend, main = x$PTNUM[[1]], lag.max = 200)})
dev.off()
  
#helper functions
signal_motion_spec <- function(x) {spectrum(x, kernel("modified.daniell", c(4,6)),
                             plot = FALSE, detrend=TRUE, pad=0.5)}

mtapPlot_multitaper <- function(y, dt, subnum=NULL, nFFT=NULL, k=5, nw=3, plot=FALSE) {
 require(pracma)
  require(multitaper)
 # y <- y - mean(y)
  
  y <- centre(y, nw=3, k=5, deltaT=2.0)
  if (is.null(nFFT)) nFFT = 2^nextpow2(length(y))
  mspec <- spec.mtm(y, k=k, nw=nw, nFFT=nFFT, plot=FALSE, centreWithSlepians = TRUE, jackknife=FALSE, returnZeroFreq=TRUE)
  plot(mspec, jackknife=FALSE)
  
  #freq is returned in 0 - 0.5 where 0.5 is nyquist freq. Divide by sampling freq to get into Hz
  #spec is the power estimate in that bin (not log transformed)
  if (plot) plot(mspec$freq/dt, 10*log10(mspec$spec), type="l", ylab="10*log10(power)", xlab="Frequency (Hz)", main=paste("Multitaper spectrum estimate for subject:", subnum))
  
  return(invisible(data.frame(f=mspec$freq/dt, power=10*log10(mspec$spec))))
}


#Raw FFTs
pdf("FFT for Right Participants.pdf", width = 11, height = 8.5)
fourierRRaw <- lapply(align_split_4Hz, function(x) {
  smoothACFR <- spectrum(x$r_ibi_interp_detrend,kernel("modified.daniell", c(8,20)),
                         plot = FALSE, detrend=TRUE, pad=0.5)
  power <- spec.pgram(smoothACFR$freq, plot = TRUE, detrend = TRUE, main = x$PTNUM[[1]], log = "no")
  fHz <- x$freq * 4
  fHz
  plots <- plot(power$freq, fHz, type = "l", main = x$PTNUM[[1]])
})
dev.off()


pdf("FFT Raw for Left Participants.pdf", width =11, height = 8.5)
fourierLRaw <- lapply(align_split_4Hz, function (x) {
  spec.pgram(x$l_ibi_interp_detrend,plot = TRUE, detrend = TRUE, main = x$PTNUM[[1]], log = "no" )
  
  
})
dev.off()



pdf("SpectralAnalysisRAW.pdf", width =11, height = 8.5)

fourierCCFRawORIGINAL <- lapply(align_split_4Hz, function(x) {
  spec.pgram(ccf, plot = TRUE, detrend = TRUE, main = i+8002, log = "no")

})

fourierCCFRaw <- lapply(align_split_4Hz, function(x) {
  spec.pgram(x, plot = TRUE, detrend = TRUE, main = x$PTNUM[[1]], log = "no")
})
dev.off()

#FFTs with some cleaning --> "subscript of out of bounds" thus, not working as of now --> also dt not specified, not being considred a ts analysis
 fourierR <- lapply(align_split_4Hz, function(x) {
  cohere <- lapply(x$r_ibi_interp_detrend, signal_motion_spec) 
  spec.pgram(cohere, plot = TRUE, detrend = TRUE, main = x$PTNUM[[1]], log = "no")
 })

 fourierL <- lapply(align_split_4Hz, function(x) {
   cohere <- lapply(x$l_ibi_interp_detrend, signal_motion_spec)
   spec.pgram(cohere, plot = TRUE, detrend = TRUE, main = x$PTNUM[[1]], log = "no" )
   
 })

  
fourierCCF <- lapply(align_split_4Hz, function(x) {
  powerSeries <- lapply(x, mtapPlot_multitaper)
  sigz <- lapply(powerSeries, signal_motion_spec)
  spec.pgram(sigz,  plot = TRUE, detrend = TRUE, log = "no")
  
})