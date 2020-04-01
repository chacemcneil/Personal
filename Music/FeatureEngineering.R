## Music notation
## The following code is intended to make it easier to calculate features of musical components
## Definitions (also found in definitions function)
##  Note:         A specific note that is not octave-specific.
##  Pitch:        An octave-specific note.
##  Key:          The musical key of the song (or section of the song). Slight differences exist for major and minor.
##  Tone:         The note relative to the key.
##  Chord:        Current chord (major, minor, 7th, etc.).
##  Root:         Tone of the root of the chord.
##  Inversion:    Inversion of chord (0 represents no inversion).
##  Tempo:        Beats per minute.
##  Meter:        Beats per measure.
##  Division:     Most number of beats per measure. 2, 3, and 4 represent duple, triple, and quadruple meters, respectively.
##  Section:      Section of a song (One of "Intro", "Verse", "Chorus", "Bridge", "Interlude", "Transition", "Ending").
##  Phrase;       Number of phrase in given section (0 when not applicable).
##  Measure:      Measure number for the song.
##  Measure_Sec:  Measure number within section.
##  Measure_Phr:  Measure number within phrase.
##  Beat:         Beat number within measure.
##  SubBeat:      Number of subdivision of beat.
##  Time:         Character representation of each time point given in the form "Measure:Beat:SubBeat"
##  Melody:       Pitch of melody line.
##  _Mel:         Relating to melody.
##  Harmony:      Pitch of harmony line.
##  _Har:         Relating to harmony.
##  Bass:         Tone of bass note. Based on Root and Inversion.
##  _Bas:         Relating to bass.
##  Interval:     A type of interval (4th, 5th, major or minor 3rd, etc.)
##  MelodyInt:    Interval between Root and tone of Melody
##  HarmonyInt:   Interval between Harmony and Melody
##  


library(data.table)
library(htmlTable)
 
#' Music Term Definition
#' 
#' Gives a basic description of all terms used in notation
#' @export
#' @example
#' ## Return a data.frame of all definitions
#' define()
#' 
#' ## Return a single definition
#' define("Chord")
#' 
#' ## Return a set of definitions
#' definte(c("Chord", "Root"))
define <- function(str = NULL) {
  terms <- c()
  defs <- c()
  dat <- data.table(Term = terms, Definition = defs)
  if(!is.null(term)) {
    dat <- dat[Term %in% str]
    if(nrow(dat) == 1) {
      dat = dat$Definition
      names(dat) <- str
    }
  }
  dat
}






# End script
