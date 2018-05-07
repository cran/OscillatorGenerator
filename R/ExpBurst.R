#' Generation of a Burst Signal with Exponential Rise and Decline
#'
#' This function takes in numeric arguments for a customizable, burst shape with exponential rise and decline. Each oscillation cycle is separated into four phases: the growth phase, in which the oscillator rises from the baseline to the peak concentration, a first drop phase, in which the oscillator declines from the peak to the secondary peak concentration, a second drop phase, in which the oscillator declines from the secondary peak to the baseline concentration and an inactive phase, in which the oscillator stays at baseline concentration. A discretized time course is returned.
#'
#' @details Standards:
#' \itemize{
#'  \item{\code{peak} and \code{sec_peak} must be larger than baseline}
#'  \item{\code{duration} must be larger than \code{resolution}}
#'  \item{\code{duration} must be a multiple of \code{resolution}}
#'  \item{\code{period} must be a multiple of \code{resolution}}
#'  \item{\code{duration}, \code{resolution}, \code{peak}, \code{sec_peak} and \code{period} must be larger than 0}
#'  \item{\code{baseline} must be larger or equal to 0}
#'  \item{\code{duty_cycle} must be larger than 0 and smaller or equal to 1}
#'  \item{\code{sec_duty_cycle} must be larger than 0 and smaller or equal to 1}
#'  \item{\code{trend} must be larger than 0}
#'  \item{\code{peak_pos} must be larger or equal to 0 and smaller than 1}
#' }
#'
#' @param baseline minimal oscillation value
#' @param peak maximal oscillation value
#' @param period oscillation period of the oscillating species (reciprocal of the frequency)
#' @param duty_cycle ratio of the active phase (oscillator above baseline) to the total oscillation period
#' @param sec_duty_cycle ratio of the primary active phase (time interval from cycle start till reaching of sec_peak) to the total active phase
#' @param sec_peak intermediary value reached after the end of the primary active phase
#' @param trend percental decrease or increase in the peak and secondary peak values for the successive oscillation cycles; if set to 1, values remain unchanged
#' @param peak_pos position of the peak value in the primary active phase (example: \code{peak_pos = 0.5}, peak position is in the middle of the primary active phase)
#' @param duration duration of the generated time course
#' @param resolution temporal resolution of the generated time course
#'
#' @examples
#' # test effect of changes in period
#' m1 = ExpBurst(baseline = 200, peak = 1000, period = 10, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m3 = ExpBurst(baseline = 200, peak = 1000, period = 200, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#'
#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#'
#' # test effect of changes in duty_cycle
#' m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.3,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.9,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)

#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#'
#' # test effect of changes in sec_duty_cycle
#' m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.3, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.6, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.9, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#'
#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#'
#' # test effect of changes in trend
#' m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 0.7, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1.3, peak_pos = 0.3, duration = 500, resolution = 0.1)
#'
#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#'
#' # test effect of changes in peak_pos
#' m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
#' m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.6, duration = 500, resolution = 0.1)
#' m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
#' sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.9, duration = 500, resolution = 0.1)
#'
#' par(mfrow = c(3,1))
#' plot(m1, type = "l", xlab = "time", ylab = "abundance")
#' plot(m2, type = "l", xlab = "time", ylab = "abundance")
#' plot(m3, type = "l", xlab = "time", ylab = "abundance")
#' @return Returns a matrix with two columns: first column time vector, second column oscillator abundance vector.
#' @export

ExpBurst <- function(baseline, peak, period, duty_cycle, sec_duty_cycle, sec_peak, trend, peak_pos, duration, resolution) {

  # check input parameters
  if(peak < baseline || sec_peak < baseline) {
    stop("(secondary) peak must be larger than baseline!")
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

  if(sec_duty_cycle <= 0 || sec_duty_cycle > 1) {
    stop("secondary duty cycle must be larger than 0 and smaller or equal to 1!")
  }

  if(trend <=0) {
    stop("trend must be larger than 0!")
  }

  if(peak_pos < 0 || peak_pos >= 1) {
    stop("peak position parameter must be larger or equal to 0 and smaller than 1!")
  }

  Osc=matrix(ncol=5,nrow=length(seq(0,duration,resolution)))
  colnames(Osc)=c("time","osc","temp", "temp2","temp3")
  Osc[,1]=seq(0,duration,resolution)
  active_tot=period*duty_cycle
  active_prim=active_tot*sec_duty_cycle
  active_peak=active_prim*peak_pos
  Osc[,3:5]=0

  for (i in 1:(nrow(Osc)*resolution/period)) {
    Osc[round((((i-1)*(period/resolution))+active_peak/resolution+1):(((i-1)*(period/resolution))+active_prim/resolution+1)),3]=seq(0,active_prim-active_peak,resolution)
    Osc[round((((i-1)*(period/resolution))+active_prim/resolution+1):(((i-1)*(period/resolution))+active_tot/resolution+1)),4]=seq(0,active_tot-active_prim,resolution)
    Osc[round((((i-1)*(period/resolution))+1):(((i-1)*(period/resolution))+active_peak/resolution+1)),5]=seq(0,active_peak,resolution)
  }

  if ((nrow(Osc)-(((i)*(period/resolution)))) <= (active_peak/resolution)) {
    Osc[round((((i)*(period/resolution))+1):nrow(Osc)),5]=
      seq(0,((nrow(Osc)-((i)*(period/resolution)))*resolution)-resolution,resolution)
  } else {
    Osc[round((((i)*(period/resolution))+1):(((i)*(period/resolution))+(active_peak)/resolution+1)),5]=
      seq(0,active_peak,resolution)

    if ((nrow(Osc)-(((i)*(period/resolution))+active_peak/resolution)) <= ((active_prim-active_peak)/resolution)) {
      Osc[round(floor(((i)*(period/resolution))+active_peak/resolution+1):nrow(Osc)),3]=
        seq(0,ceiling(nrow(Osc)-(((i)*(period/resolution))+active_peak/resolution))*resolution-resolution,resolution)
    } else {
      Osc[round((((i)*(period/resolution))+active_peak/resolution+1):(((i)*(period/resolution))+active_prim/resolution+1)),3]=
        seq(0,active_prim-active_peak,resolution)
    }

    if ((nrow(Osc)-(((i)*(period/resolution))+active_prim/resolution)) <= ((active_tot-active_prim)/resolution)) {
      Osc[round(floor(((i)*(period/resolution))+active_prim/resolution+1):nrow(Osc)),4]=
        seq(0,ceiling(nrow(Osc)-(((i)*(period/resolution))+active_prim/resolution))*resolution-resolution,resolution)
    } else {
      Osc[round((((i)*(period/resolution))+active_prim/resolution+1):(((i)*(period/resolution))+active_tot/resolution+1)),4]=
        seq(0,active_tot-active_prim,resolution)
    }
  }

  for (i in 1:(nrow(Osc)*resolution/period)) {
    Osc[round((((i-1)*(period/resolution))+1):(((i-1)*(period/resolution))+active_peak/resolution+1)),2]=
      baseline*exp((log(peak/baseline)/active_peak)*Osc[round((((i-1)*(period/resolution))+1):(((i-1)*(period/resolution))+active_peak/resolution+1)),5])

    Osc[round((((i-1)*(period/resolution))+active_peak/resolution+1):(((i-1)*(period/resolution))+active_prim/resolution+1)),2]=
      peak*exp((log(sec_peak/peak)/(active_prim-active_peak))*Osc[round((((i-1)*(period/resolution))+active_peak/resolution+1):(((i-1)*(period/resolution))+active_prim/resolution+1)),3])

    Osc[round((((i-1)*(period/resolution))+active_prim/resolution+1):(((i-1)*(period/resolution))+active_tot/resolution+1)),2]=
      sec_peak*exp((log(baseline/sec_peak)/(active_tot-active_prim))*Osc[round((((i-1)*(period/resolution))+active_prim/resolution+1):(((i-1)*(period/resolution))+active_tot/resolution+1)),4])
    peak=peak*trend
    sec_peak=sec_peak*trend
    if(peak <= baseline) {
      peak=baseline
    }
    if(sec_peak <= baseline) {
      sec_peak=baseline
    }
  }

  if ((nrow(Osc)-(((i)*(period/resolution)))) <= (active_peak/resolution)) {
    Osc[round((((i)*(period/resolution))+1)):round(nrow(Osc)),2]=
      baseline*exp((log(peak/baseline)/active_peak)*Osc[round((((i)*(period/resolution))+1)):round(nrow(Osc)),5])
  } else {
    Osc[round((((i)*(period/resolution))+1)):round((((i)*(period/resolution))+active_peak/resolution)),2]=
      baseline*exp((log(peak/baseline)/active_peak)*Osc[round((((i)*(period/resolution))+1)):round((((i)*(period/resolution))+active_peak/resolution)),5])

    if ((nrow(Osc)-(((i)*(period/resolution))+active_peak/resolution)) <= ((active_prim-active_peak)/resolution)) {
      Osc[round(floor(((i)*(period/resolution))+active_peak/resolution+1)):round(nrow(Osc)),2]=
        peak*exp((log(sec_peak/peak)/(active_prim-active_peak))*Osc[round(floor(((i)*(period/resolution))+active_peak/resolution+1)):round(nrow(Osc)),3])
    } else {
      Osc[round((((i)*(period/resolution))+active_peak/resolution+1)):round((((i)*(period/resolution))+active_prim/resolution)),2]=
        peak*exp((log(sec_peak/peak)/(active_prim-active_peak))*Osc[round((((i)*(period/resolution))+active_peak/resolution+1)):round((((i)*(period/resolution))+active_prim/resolution)),3])
      if ((nrow(Osc)-(((i)*(period/resolution))+active_prim/resolution)) <= ((active_tot-active_prim)/resolution)) {
        Osc[round(floor(((i)*(period/resolution))+active_prim/resolution+1)):round(nrow(Osc)),2]=
          sec_peak*exp((log(baseline/sec_peak)/(active_tot-active_prim))*Osc[round(floor(((i)*(period/resolution))+active_prim/resolution+1)):round(nrow(Osc)),4])

      } else {
        Osc[round((((i)*(period/resolution))+active_prim/resolution+1)):round((((i)*(period/resolution))+active_tot/resolution)),2]=
          sec_peak*exp((log(baseline/sec_peak)/(active_tot-active_prim))*Osc[round((((i)*(period/resolution))+active_prim/resolution+1)):round((((i)*(period/resolution))+active_tot/resolution)),4])
      }
    }
  }
  Osc[which(is.na(Osc[,2])),2]=baseline
  Osc=Osc[,1:2]

  return(Osc)
}
