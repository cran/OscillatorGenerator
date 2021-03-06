% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExpBurst.R
\name{ExpBurst}
\alias{ExpBurst}
\title{Generation of a Burst Signal with Exponential Rise and Decline}
\usage{
ExpBurst(baseline, peak, period, duty_cycle, sec_duty_cycle, sec_peak, trend,
  peak_pos, duration, resolution)
}
\arguments{
\item{baseline}{minimal oscillation value}

\item{peak}{maximal oscillation value}

\item{period}{oscillation period of the oscillating species (reciprocal of the frequency)}

\item{duty_cycle}{ratio of the active phase (oscillator above baseline) to the total oscillation period}

\item{sec_duty_cycle}{ratio of the primary active phase (time interval from cycle start till reaching of sec_peak) to the total active phase}

\item{sec_peak}{intermediary value reached after the end of the primary active phase}

\item{trend}{percental decrease or increase in the peak and secondary peak values for the successive oscillation cycles; if set to 1, values remain unchanged}

\item{peak_pos}{position of the peak value in the primary active phase (example: \code{peak_pos = 0.5}, peak position is in the middle of the primary active phase)}

\item{duration}{duration of the generated time course}

\item{resolution}{temporal resolution of the generated time course}
}
\value{
Returns a matrix with two columns: first column time vector, second column oscillator abundance vector.
}
\description{
This function takes in numeric arguments for a customizable, burst shape with exponential rise and decline. Each oscillation cycle is separated into four phases: the growth phase, in which the oscillator rises from the baseline to the peak concentration, a first drop phase, in which the oscillator declines from the peak to the secondary peak concentration, a second drop phase, in which the oscillator declines from the secondary peak to the baseline concentration and an inactive phase, in which the oscillator stays at baseline concentration. A discretized time course is returned.
}
\details{
Standards:
\itemize{
 \item{\code{peak} and \code{sec_peak} must be larger than baseline}
 \item{\code{duration} must be larger than \code{resolution}}
 \item{\code{duration} must be a multiple of \code{resolution}}
 \item{\code{period} must be a multiple of \code{resolution}}
 \item{\code{duration}, \code{resolution}, \code{peak}, \code{sec_peak} and \code{period} must be larger than 0}
 \item{\code{baseline} must be larger or equal to 0}
 \item{\code{duty_cycle} must be larger than 0 and smaller or equal to 1}
 \item{\code{sec_duty_cycle} must be larger than 0 and smaller or equal to 1}
 \item{\code{trend} must be larger than 0}
 \item{\code{peak_pos} must be larger or equal to 0 and smaller than 1}
}
}
\examples{
# test effect of changes in period
m1 = ExpBurst(baseline = 200, peak = 1000, period = 10, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m3 = ExpBurst(baseline = 200, peak = 1000, period = 200, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)

par(mfrow = c(3,1))
plot(m1, type = "l", xlab = "time", ylab = "abundance")
plot(m2, type = "l", xlab = "time", ylab = "abundance")
plot(m3, type = "l", xlab = "time", ylab = "abundance")

# test effect of changes in duty_cycle
m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.3,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.9,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
par(mfrow = c(3,1))
plot(m1, type = "l", xlab = "time", ylab = "abundance")
plot(m2, type = "l", xlab = "time", ylab = "abundance")
plot(m3, type = "l", xlab = "time", ylab = "abundance")

# test effect of changes in sec_duty_cycle
m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.3, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.6, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.9, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)

par(mfrow = c(3,1))
plot(m1, type = "l", xlab = "time", ylab = "abundance")
plot(m2, type = "l", xlab = "time", ylab = "abundance")
plot(m3, type = "l", xlab = "time", ylab = "abundance")

# test effect of changes in trend
m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 0.7, peak_pos = 0.3, duration = 500, resolution = 0.1)
m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1.3, peak_pos = 0.3, duration = 500, resolution = 0.1)

par(mfrow = c(3,1))
plot(m1, type = "l", xlab = "time", ylab = "abundance")
plot(m2, type = "l", xlab = "time", ylab = "abundance")
plot(m3, type = "l", xlab = "time", ylab = "abundance")

# test effect of changes in peak_pos
m1 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.3, duration = 500, resolution = 0.1)
m2 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.6, duration = 500, resolution = 0.1)
m3 = ExpBurst(baseline = 200, peak = 1000, period = 100, duty_cycle = 0.6,
sec_duty_cycle = 0.5, sec_peak = 850, trend = 1, peak_pos = 0.9, duration = 500, resolution = 0.1)

par(mfrow = c(3,1))
plot(m1, type = "l", xlab = "time", ylab = "abundance")
plot(m2, type = "l", xlab = "time", ylab = "abundance")
plot(m3, type = "l", xlab = "time", ylab = "abundance")
}
