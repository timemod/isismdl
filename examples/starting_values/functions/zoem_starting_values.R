# dependency_structure: als je uit model$get_dep_struct() krijgt,
# doet vervolgens:
#
# lhs             rhs              lag
# pc____expr      mltdummybn       "0"
# pc_cepf_pr        c__pp_hu "-2 -1 0"
# pc_cepf_pr      pc_mevt_pr      "-1"
#
# ->
#
# lhs             rhs              lag
# pc____expr      mltdummybn         0
# pc_cepf_pr        c__pp_hu         0
# pc_cepf_pr        c__pp_hu        -1
# pc_cepf_pr        c__pp_hu        -2
# pc_cepf_pr      pc_mevt_pr        -1
parse_dependency_structure <- function(dependency_structure) {
  dependency_structure |>
    mutate(lag = gsub('"', "", lags)) |> # drop quotes
    separate_rows(lag, sep = "\\s+") |> # split on spaces
    mutate(lag = as.numeric(lag)) |> # make numeric
    select(-lags) |> # drop lags column
    arrange(desc(lag)) # sort lags 0, -1, -2
}

# alle (variabelen jaar) paren die een afhankelijkheid op variabele var in jaar year hebben
.get_dependents <- function(var, year, dependency_structure) {
  # dependency_structure: dataframe met kolommen lhs rhs lag
  dependency_structure |>
    filter(rhs == var) |>
    mutate(year = year - lag) |> # nolint lhs y afhangt van rhs x met lag -2 en x is het jaar 2013, dan hangt y in 2013 - -2 = 2015 dus af van x in 2013
    select(c("lhs", "year")) |>
    rename(var = lhs)
}

.find_dependency_paths <- function(paths, year, observable_var, dependency_structure) {
  finished_paths <- list()
  new_paths <- list()

  for (path in paths) {
    last_entry <- path |> dplyr::slice(dplyr::n())
    last_var <- last_entry$var
    last_year <- last_entry$year

    # het pad eindigt al op observable_var of heeft een te hoog jaar,
    # dan hoeven we het pad niet uit te breiden
    if (last_var == observable_var || last_year > year) {
      finished_paths <- append(finished_paths, list(path))
      next
    }

    dependents <- .get_dependents(var = last_var, year = last_year, dependency_structure = dependency_structure)

    # de laatste variabele van het pad heeft geen variabelen die van hem afhangen,
    # dan hoeven we het pad niet later uit te breiden
    if (nrow(dependents) == 0) {
      finished_paths <- append(finished_paths, list(path))
      next
    }

    for (i in seq_len(nrow(dependents))) {
      dependent <- dependents[i, ]
      dependent_var <- dependent$var
      dependent_year <- dependent$year

      path_with_one_more_step <- rbind(path, list(dependent_var, dependent_year))
      new_paths <- append(new_paths, list(path_with_one_more_step))
    }
  }

  if (length(paths) == length(finished_paths)) {
    .filter_paths(
      paths = paths,
      observable_var = observable_var,
      year = year
    )
  } else {
    append(
        .filter_paths(
          paths = finished_paths,
          observable_var = observable_var,
          year = year
        ),
        .find_dependency_paths(paths = new_paths, year = year, observable_var = observable_var, dependency_structure = dependency_structure)
      )
  }
}

.filter_paths <- function(paths, observable_var, year) {
  # filter paden eruit die niet op observable_var eindigen,
  # dat zijn geen paden van afhankelijkheden beginnend met het argument paths
  # en eindigend bij observable_var
  filtered_paths <- list()

  for (path in paths) {
    last_entry <- path |> dplyr::slice(dplyr::n())
    last_var <- last_entry$var
    last_year <- last_entry$year

    if (last_var != observable_var || last_year > year) {
      next
    } else {
      filtered_paths <- append(filtered_paths, list(path))
    }
  }

  filtered_paths
}


.find_dependencies_single_var <- function(var,
                                          year,
                                          vars,
                                          min_year,
                                          dependency_structure) {
  var_data <- vars[vars$var == var & vars$year == year, ] # filter werkt om de een of andere reden niet

  if (nrow(var_data) == 0) {
    stop(paste("BUG! Je wilt de dependencies van", var, "in het jaar", year, "bekijken, maar deze zit niet in", vars))
  }

  var_source <- var_data[[1, "source"]]

  if (var_source != "unknown") {
    return(vars)
  }

  if (year < min_year) {
    vars$source[vars$var == var & vars$year == year] <- "missing"
    return(vars)
  }

  dependencies <- dependency_structure |> filter(lhs == var)

  if (nrow(dependencies) == 0) {
    vars$source[vars$var == var & vars$year == year] <- "no dependencies"
    return(vars)
  }

  number_of_known_dependencies <- 0

  for (j in seq_len(nrow(dependencies))) {
    dependency_row <- dependencies[j, ]
    dependency_var <- dependency_row[["rhs"]]
    dependency_year <- dependency_row[["lag"]] + year

    # check of een element met deze var en year al in vars zit! dit is het geval van niet
    if (nrow(vars[vars$var == dependency_var & vars$year == dependency_year, ]) == 0) {
      vars <- rbind(vars, list(dependency_var, dependency_year, "unknown"))
    }

    vars <- .find_dependencies_single_var(
      var = dependency_var,
      year = dependency_year,
      vars = vars,
      min_year = min_year,
      dependency_structure = dependency_structure
    )

    dependency_var_data <- vars[vars$var == dependency_var, ] |> filter(year == dependency_year)
    dependency_var_source <- dependency_var_data[[1, "source"]]

    if (dependency_var_source == "unknown") {
      # the var waarvan we de dependencies zoeken door find_dependencies_single_var aan te roepen,
      # moet altijd met een source die niet unknown is uit de functie komen
      stop("BUG!!")
    }

    if (dependency_var_source != "missing" && dependency_var_source != "incalculable") {
      number_of_known_dependencies <- number_of_known_dependencies + 1

      if (dependency_var_source == "in data") {
        vars$source[vars$var == dependency_var & vars$year == dependency_year] <- "in data, used"
      }
    }
  }

  if (number_of_known_dependencies == nrow(dependencies)) {
    vars$source[vars$var == var & vars$year == year] <- "calculable"
  } else {
    vars$source[vars$var == var & vars$year == year] <- "incalculable"
  }

  vars
}

# vars: var year source
# min_year: numeric
# dependency_structure: als uit parse_dependency_structure
.find_dependencies <- function(vars, min_year, dependency_structure) {
  while (any(vars$source == "unknown")) {
    row <- (vars |> filter(source == "unknown"))[1, ]

    variable <- row[["var"]]
    year <- row[["year"]]
    source <- row[["source"]]

    vars <- .find_dependencies_single_var(
      var = variable,
      year = year,
      min_year = min_year,
      vars = vars,
      dependency_structure = dependency_structure
    )
  }

  vars
}

# dependency_path: var year
# data: var year value
# min_year: numeric
# dependency_structure als uit parse_dependency_structure
# return: var year source
find_dependencies_of_paths <- function(dependency_paths, data_init, min_year,
                                       dependency_structure) {

  data <- regts::as_data_frame(data_init, format = "long",
                               name_col = "var", period_col = "year") |>
    mutate(year = as.numeric(year)) |>
    filter(!is.na(value))

  # data: var year value
  data <- data |>
    select(c("var", "year")) |>
    mutate(source = "in data")

  # data: var year source

  # vars: var year
  vars <- tibble()

  # dependency_paths list(df), var year
  for (path in dependency_paths) {
    last_entry <- path |> dplyr::slice(dplyr::n())
    observed_year <- last_entry$year
    observed_var <- last_entry$var

    # als deze in de data zit, wat zo zou moeten zijn, moeten we hem hier er uit filteren, we willen deze op unknown hebben,
    # want zo gaan we verderop hem in de vars stoppen waarvan we de dependencies onderzoeken
    data <- data |> filter(!(var == observed_var & year == observed_year))

    vars <- rbind(vars, path)
  }

  vars <- dplyr::distinct(vars)
  vars["source"] <- "unknown"

  # we kijken of de variabelen uit de pad in de data zitten
  # dat mag alleen zo zijn voor de laatste variabele van het pad,
  # dat is de geobserveerde waarde,
  # en die hebben we al uit de data gefilterd
  vars_in_data <- dplyr::inner_join(vars, data,
    by = dplyr::join_by(var == var, year == year)
  )

  if (nrow(vars_in_data) > 0) {
    print(vars_in_data)
    stop(paste("Je wilt startwaarden afleiden uit andere waarden maar er zijn variabelen die je dan moet uitrekenen,
                die je als data hebt meegegeven, zie boven"))
  }

  .find_dependencies(vars = dplyr::bind_rows(vars, data), min_year = min_year, dependency_structure = dependency_structure)
}

# nolint dependency structure: lhs rhs lag
# nolint endogenous_variables: list[chr]
# nolint return: list[chr]
get_equations_with_no_dependencies <- function(endogenous_variables, dependency_structure) {
  setdiff(endogenous_variables, dependency_structure$lhs)
}
