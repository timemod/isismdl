rm(list = ls())
library(conflicted)
library(regts)
library(dplyr)
library(tibble)
library(tidyr)
library(igraph)
library(stringr)
conflicts_prefer(dplyr::filter)

source("examples/starting_values/functions/zoem_starting_values.R")
source("examples/starting_values/functions/isismdl_starting_values.R")

model_file <- "examples/starting_values/toy_model3.mdl"
model <- isismdl::isis_mdl(model_file)

struc <- model$get_dep_struct()
dependency_structure <- parse_dependency_structure(
  dependency_structure = model$get_dep_struct()
)

# Ik ga nu even uit dat je startwaarden wilt afleiden in 1 jaar
# Met een loopje kun je dat voor meerdere jaren doen,
# dan werk je natuurlijk van achter naar voor, oude jaren het eerst.
year_derive_start_values <- 2021

# Dit geeft alle paden hoe je van een (variabele, jaar) paar uit paths naar (observed_variable, year) uit kunt komen via de vergelijkingen van het model
# Je krijgt een lijst van dataframes terug
# Elk dataframe is een pad, de volgorde van de rijen is dus belangrijk.
# Stel je hebt een vergelijking y = x[-1] + 7 en je wilt y in 2027 bekijken,
# dan heb je dus een pad (x, 2026) -> (y, 2027)
#
# Als we dit hebben, dan weten we welke vergelijkingen (die van dependency_paths)
# we moeten kunnen draaien om derived_variable te fitten op observed_variable

dependency_paths <- .find_dependency_paths(
  paths =  list(
    data.frame(
      var = "bond",
      year = year_derive_start_values
    )),
  # TODO als het lukt om geobserveerde en afleidbare variabelen uit verschillende jaren te laten werken,
  # voeg deze data toe aan startwaarden data,
  # daar moet je dus niet alleen zeggen "fit a op x", maar "fit a uit 2021 op x in 2024"
  year = year_derive_start_values,
  observable_var = "management_fee_quote",
  dependency_structure = dependency_structure
)

print(dependency_paths)

dependency_paths <- find_dependency_paths_isismdl("bond", "management_fee_quote",
                                                  dependency_structure = dependency_structure)
print(dependency_paths)
