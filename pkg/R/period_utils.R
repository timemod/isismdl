get_period_indices <- function(mdl_period, period) {
  mdl_period_start <- start_period(mdl_period)
  startp <- as.integer(start_period(period) - mdl_period_start + 1)
  endp   <- as.integer(end_period(period)   - mdl_period_start + 1)
  return(list(startp = startp, endp = endp))
}
