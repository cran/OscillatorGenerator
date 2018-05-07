#' Generation of a Square-wave Signal
#'
#' This function takes in numeric arguments for a customizable, square-wave spike shape. A discretized time course is returned.
#'
#' @details Standards:
#' \itemize{
#'  \item{\code{peak} must be larger than \code{baseline}}
#'  \item{\code{duration} must be larger than \code{resolution}}
#'  \item{\code{duration} must be a multiple of \code{resolution}}
#'  \item{\code{period} must be a multiple of \code{resolution}}
#'  \item{\code{duration}, \code{resolution}, \code{peak} and \code{period} must be larger than 0}
#'  \item{\code{baseline} must be larger or equal to 0}
#'  \item{\code{duty_cycle} must be larger than 0 and smaller or equal to 1}
#'  \item{\code{trend} must be larger than 0}
#' }
#'
#' @param baseline minimal oscillation value
#' @param peak maximal oscillation value
#' @param period oscillation period of the oscillating species (reciprocal of the frequency)
#' @param duty_cycle ratio of the active phase (oscillator above baseline) to the total oscillation period
#' @param trend percental decrease or increase in the peak value for the successive oscillation cycles; if set to 1, peak value remains unchanged
#' @param duration duration of the generated time course
#' @param resolution  temporal resolution of the generated time course
#' @examples
#' # test effect of changes in period
#' m1 = SquareSpike(baseline = 200, peak = 1000, period = 50, duty_cycle = 0.6,
#' trend = 1, duration = 500, resolution = 0.1)
#' m2 = SquareSpike(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' trend = 1, duration = 500, resolution = 0.1)
#' m3 = SquareSpike(baseline = 200, peak = 1000, period = 200, duty_cycle = 0.6,
#' trend = 1, duration = 500, resolution = 0.1)
#'
#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#'
#' # test effect of changes in duty_cycle
#' m1 = SquareSpike(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.3,
#' trend = 1, duration = 500, resolution = 0.1)
#' m2 = SquareSpike(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' trend = 1, duration = 500, resolution = 0.1)
#' m3 = SquareSpike(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.9,
#' trend = 1, duration = 500, resolution = 0.1)
#'
#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#'
#' # test effect of changes in trend
#' m1 = SquareSpike(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' trend = 0.7, duration = 500, resolution = 0.1)
#' m2 = SquareSpike(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' trend = 1, duration = 500, resolution = 0.1)
#' m3 = SquareSpike(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' trend = 1.3, duration = 500, resolution = 0.1)
#'
#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#' @return Returns a matrix with two columns: a time vector and an oscillator abundance vector.
#' @export
#'

SquareSpike <- function(baseline, peak, period, duty_cycle, trend, duration, resolution) {

  # check input parameters
  if(peak < baseline) {
    stop("peak must be larger than baseline!")
  }

  if(duration <= resolution ) {
    stop("duration must be longer than resolution!")
  }

  if((abs(duration/resolution - round(duration/resolution))) > 1e-10) {
    stop("duration must be a multiple of resolution!")
  }

  if((abs(period/resolution - round(period/resolution))) > 1e-10) {
    stop("period must be a multiple of resolution!")
  }

  if(duration <= 0 || resolution <= 0 || peak <= 0  || period <= 0) {
    ID=matrix(ncol=1,nrow=6)
    rownames(ID)=c("duration","resolution","peak","period")
    ID[,1]=c(duration,resolution,peak,period)
    Ind=which(ID[,1]<=0)
    stop(paste0(rownames(ID)[Ind]," must be larger than 0! "))
  }

  if(baseline < 0) {
    stop("baseline must be larger than or equal to 0!")
  }


  if(duty_cycle <= 0 || duty_cycle > 1) {
    stop("duty cycle must be larger than 0 and smaller or equal to 1!")
  }

  if(trend <=0) {
    stop("trend must be larger than 0!")
  }


  Osc=matrix(ncol=2,nrow=length(seq(0,duration,resolution)))
  colnames(Osc)=c("time","osc")
  Osc[,1]=seq(0,duration,resolution)
  active=period*duty_cycle
  Osc[,2]=baseline
  for (i in 1:(nrow(Osc)*resolution/period)) {
    Osc[round((((i-1)*(period/resolution))+2):(((i-1)*(period/resolution))+active/resolution)),2]=peak

    peak=peak*trend
    if(peak <= baseline) {
      peak=baseline
    }
  }

  if ((nrow(Osc)-(((i)*(period/resolution)))) <= (active/resolution)) {
    Osc[round((((i)*(period/resolution))+1):nrow(Osc)),2]=peak
  } else {
    Osc[round((((i)*(period/resolution))+1):(((i)*(period/resolution))+active/resolution)),2]=peak
  }
  Osc[round((((i)*(period/resolution))+1)),2] = baseline
  return(Osc)
}
