ensure_solve_df_cols <- function(df, default_initial_guess = 0.1) {
  required_cols <- c("solve_period", "observed_variable", "solve_variable")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      "Make sure the following column(s) exist in df: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (!("group" %in% colnames(df))) {
    df$group <- paste0("group_", seq_len(nrow(df)))
    # TODO: no message if argument report= "no"?
    message(
      "There were no groups in the given tibble,\n",
      "so each solve and derived variable are treated separately"
    )
  }
  if (!("initial_guess" %in% colnames(df))) {
    df$initial_guess <- rep(default_initial_guess, nrow(df))
    message(
      "No initial_guess is given, so for each entry the initial guess is ",
      default_initial_guess
    )
    return(df)
  }

  ig <- df$initial_guess
  if (!all(is.numeric(ig) | is.na(ig))) {
    stop(
      "Use only numerical or NA values in the initial_guess column.\n",
      "Offending values at rows: ",
      paste(which(!(is.numeric(ig) | is.na(ig))), collapse = ", ")
    )
  }

  ig_clean <- ifelse(is.na(ig) | ig == 0, default_initial_guess, ig)
  df$initial_guess <- ig_clean
  return(df)
}
