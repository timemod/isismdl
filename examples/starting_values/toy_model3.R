# nolint start

# De functies staan gedefinieerd in zoem_starting_values.R
# Er zijn wat tests in test_zoem_starting_values.R
# Verder staan er in demo\startwaarden 3 oefen modellen bestanden

# Het gaat om startwaarden afleiden
# Ik ga er telkens van uit dat je model geen leads heeft, dat is bij ZOEM ook niet
#
# Stel je hebt een simpel demografisch model
#
# totaal = mannen + vrouwen
# mannen = oude_mannen + jonge_mannen
# oude_mannen = jonge_mannen[-1] - gestorven_mannen
# vrouwen = oude_vrouwen + jonge_vrouwen
# oude_vrouwen = jonge_vrouwen[-1] - gestorven_vrouwen
#
# groei = totaal - totaal[-1]
# groei_vrouwen = vrouwen - vrouwen[-1]
# groei_mannen = mannen - mannen[-1]
#
# Het gaat er even niet om dat ik dit model makkelijker kan herschrijven
# Wil ermee rekenen, dan heb ik startwaarden voor jonge_mannen, jonge_vrouwen, mannen, vrouwen en totaal nodig
# Maar als ik totaal en mannen weet, dan ligt vrouwen ook vast
# We zeggen dan dat totaal en mannen geobserveerd zijn en vrouwen afleidbaar
# Vrouwen kunnen we dus automatisch uitrekenen
# Dat is ook een verstandig idee,
# want Isis gaat gewoon lekker rekenen als ik totaal = 200, mannen = 150 en vrouwen = 140 meegeef
#
# Wat zijn dan moelijkheden als ik dit echt wil doen?
#
# Zeg dat het lag jaar 2021 is.
# Stel ik heb totaal en mannen uit 2021 in mijn data, dan is het simpel.
# Maar misschien heb ik dat niet, maar wel oude_mannen 2021 en jonge_mannen 2021.
# Dan kan ik ook vrouwen afleiden.
# Maar ook als ik in plaats daarvan jonge_mannen 2020, jonge_mannen 2021 en gestorven_mannen 2021 heb.
# Dat moet je dus automatisch nagaan, is mijn data voldoende om te kunnen rekenen.
#
# Verder klaagt Isis bij het oplossen als hij lags mist.
# Als we dus een verdere vergelijking popcorn = mannen[-1] + popcorn[-1] hebben,
# die er verder helemaal niet met vrouwen afleiden te maken heeft,
# dan hebben we alsnog een probleem als de lag van popcorn mist.
# We willen dus uberhaupt de popcorn vergelijking niet draaien.
# We kunnen de vergelijking popcorn deactiveren, maar dan nog klaagt Isis over missende lags.
# Dus vullen we dan gewoon een getal in bij popcorn waar we verder niks mee doen.
#
# We kunnen volgens mij niet een startwaarde afleiden uit een geobserveerde waarde uit een later jaar
# Je kunt namelijk niet vergelijkingen voor een specifiek jaar uitzetten.
# Een vergelijking is altijd of helemaal uit, of aan voor alle jaren.
# Een klein voorbeeld:
#
#
# a = z[-1]
# z = y[-1]
# y = x[-1]
#
# x = b
#
# Dan kan ik z in 2021 afleiden uit x in 2019, (x, 2019) -> (y, 2020) -> (z, 2021)
# Maar als ik probeer te fitten in 2021 op x, dan wil Isis niet.
# Dit is anders dan het popcorn voorbeeld, waar ik de errors kan laten verdwijnen, als ik de popcorn vergelijking uitzet.
# Hier is er geen enkele vergelijking die ik uit kan zetten om het te laten werken.
# Ik kwam pas gaandeweg achter dit probleem.
#
# Gelukkig is dezelfde jaar moeten zijn geen belemmering, want dit hebben we bij ZOEM niet nodig.
# Verder moet dit wel technisch mogelijk zijn, als je ook intern in Isis aanpassingen maakt.

library(conflicted)
library(regts)
library(dplyr)
library(tibble)
library(tidyr)
conflicts_prefer(dplyr::filter)

source("examples/starting_values/functions/zoem_starting_values.R")

fit_options <- list(
  maxiter = 20,
  cvgrel = 30,
  mkdcrt = 0.06
)

# Hier zeg ik: de waarde van bond kan ik uitrekenen als ik management_fee_quote weet
# Kijk maar in het model
# Als ik stock en bond in 2021 weet, dan weet ik assets.
# Dan weet ik ook management_fee in 2021 net als total_fee en management_fee_quote in 2021
# Dus uit management_fee_quote uit 2021 en voldoende andere data, kan ik bond in 2021 afleiden
starting_values_variables_df <- tribble(
  ~observed_variable,    ~derived_variable,
  "management_fee_quote", "bond"
)

# Check of de startwaarden variabelen zinnig zijn
# We willen geen dubbele namen of overlap tussen de geobserveerden en afleidbaren
# Dat slaat functioneel nergens op
if (
  !length(starting_values_variables_df$observed_variable) == length(unique(starting_values_variables_df$observed_variable)) ||
    !length(starting_values_variables_df$derived_variable) == length(unique(starting_values_variables_df$derived_variable)) ||
    length(intersect(starting_values_variables_df$derived_variable, (starting_values_variables_df$derived_variable) > 0))
) {
  stop()
}

data_init <- read_ts_xlsx("examples/starting_values/data_init.xlsx")

model_file <- "examples/starting_values/toy_model3.mdl"
model <- isismdl::isis_mdl(model_file)

model$init_data(data = data_init, data_period = "2019/2027")

# Geeft de afhankelijkheden van de variabelen weer.
# Als je een vergelijking y = x[-1] + 2 * x[-2] + z + 9 hebt,
# dan bevat dependency_structure dus onder andere de rijen:
#
# lhs rhs lag
# y   x   -2
# y   x   -1
# y   z    0

struc <- model$get_dep_struct()
dependency_structure <- parse_dependency_structure(
  dependency_structure = model$get_dep_struct()
)

# Ik ga nu even uit dat je startwaarden wilt afleiden in 1 jaar
# Met een loopje kun je dat voor meerdere jaren doen,
# dan werk je natuurlijk van achter naar voor, oude jaren het eerst.
year_derive_start_values <- 2021

# Hierboven hebben we alle input voor het algoritme klaargezet.
# Beneden moet dan de functionaliteit om startwaarden af te leiden komen.

starting_values_df <- tibble(variable = character(), year = numeric(), value = numeric())

# Eerst loop ik over de geobserveerde startwaarde en bijbehorende te fitten startwaarden.
# Voor elk van die paren gaan we eerst eens alle mogelijke paden van afleidbare startwaarde naar de geobserveerde startwaarde berekenen.
# Vervolgens kijken we, of er genoeg data aanwezig is om al die vergelijkingen uit te rekenen.
# Daarne begint het echte fitten

# We gaan geobserveerde en afleidbare paren dus 1 voor 1 na.
# Dat maakt het makkelijker te debuggen en stap voor stap in de echt ZOEM in te bouwen.
# Dan kun je namelijk de variabelen 1 voor 1 uit de oude methode halen en met de nieuwe methode doen.
# Dat werkt uiteraard NIET als er interacties tussen de te fitten variabelen zijn.
# Dan moet je wel in 1 keer alle variabelen fitten.
# Dat is een kleine aanpassing in de code.
# Je moet dan niet direct stoppen als je pad bij een geobserveerde variabele uitkomt.
# Je moet doorgaan tot het jaar te hoog is.
# Er kan namelijke een geobserveerde variabele van een andere geobserveerde variabele afhangen.
# verder fit je dan niet 1 voor 1 maar ook alle variabelen tegelijk.
for (i in seq_len(nrow(starting_values_variables_df))) {
  row <- starting_values_variables_df[i, ]
  observed_variable <- row$observed_variable
  derived_variable <- row$derived_variable

  paths <- data.frame(
    var = derived_variable,
    year = year_derive_start_values
  )

  # Dit geeft alle paden hoe je van een (variabele, jaar) paar uit paths naar (observed_variable, year) uit kunt komen via de vergelijkingen van het model
  # Je krijgt een lijst van dataframes terug
  # Elk dataframe is een pad, de volgorde van de rijen is dus belangrijk.
  # Stel je hebt een vergelijking y = x[-1] + 7 en je wilt y in 2027 bekijken,
  # dan heb je dus een pad (x, 2026) -> (y, 2027)
  #
  # Als we dit hebben, dan weten we welke vergelijkingen (die van dependency_paths)
  # we moeten kunnen draaien om derived_variable te fitten op observed_variable
  dependency_paths <- .find_dependency_paths(
    paths = list(paths),
    # TODO als het lukt om geobserveerde en afleidbare variabelen uit verschillende jaren te laten werken,
    # voeg deze data toe aan startwaarden data,
    # daar moet je dus niet alleen zeggen "fit a op x", maar "fit a uit 2021 op x in 2024"
    year = year_derive_start_values,
    observable_var = observed_variable,
    dependency_structure = dependency_structure
  )

  #print(paths)
  #print(dependency_structure)
  #print(dependency_paths)


  # Gegeven een reeks paden dependency_paths en een dataframe data,
  # kijken we of de juiste data aanwezig is om met de vergelijkingen van het model alle vergelijkingen uit dependency_paths te berekenen.
  # We kijken daarbij niet verder terug dan min_year.
  # Je krijgt een dataframe terug die er als volgt uit ziet:
  #
  # var                   year source
  #
  # bond                  2021 in data
  # assets                2021 calculable
  # management_fee        2022 calculable
  # management_fee_quote  2023 calculable
  # total_fee             2022 calculable
  # dstock                2022 calculable
  # stock                 2022 calculable
  #
  # "in data" betekent dat dit (variabele, jaar) paar in de data aanwezig is,
  # maar niet nodig is om 1 van de vergelijkingen uit dependency_paths uit te rekenen.
  #
  # "in data, used" betekent dat dit (variabele, jaar) paar in de data aanwezig is,
  # en nodig is om 1 van de vergelijkingen uit dependency_paths uit te rekenen.
  #
  # "no dependencies" beteken dat een (variabele, jaar) paar nergens van afhangt,
  # dat moet dus een vergelijking van de vorm x = 8 zijn
  #
  # "incalculable" betekent dat een (variabele, jaar) paar niet uit te rekenen is gegeven de vergelijkingen en aanwezige data,
  # als we zijn afhankelijkheden allemaal terug volgen tot min_year, dan mist er tenminste 1 waarde
  #
  # "missing" betekent dat een (variabele, jaar) paar ten minste voor 1 variabele ergens op een pad nodig is om uit te rekenen,
  # maar hij niet in de data zit en zijn jaar lager is dan min_year
  #
  # "unknown" betekent dat van een (variabele, jaar) paar nog niet bekend is in welke van bovenstaande categorieen hij valt
  # dat zou je niet terug moeten krijgen als je find_dependencies_of_paths aanroept, dat zit alleen intern als er geen bugs zijn
   all_dependencies <- find_dependencies_of_paths(
    dependency_paths = dependency_paths,
    data_init = data_init,
    dependency_structure = dependency_structure,
    min_year = year_derive_start_values - 1
  )

  # TODO als de code eenmaal af is, dan wil je hier een error gooien,
  # als er in all_dependencies een variabele is aangemerkt als incalculable of missing.
  # In dat geval is de data niet voldoende om gegeven de vergelijkingen de startwaarden af te leiden
  # De gebruiker heeft zicht vergist.
  # Nota bene, Rob vroeg zich af, hoe je omgaat niet unieke oplossingen
  # Als dit komt doordat je minder observaties dan vrije variabelen hebt,
  # dan is dat een fout van de gebruiker.
  # Dit is simpelweg niet de bedoeling.
  # De startwaarden moeten uniek zijn vastgelegd zoals in het mannen en vrouwen voorbeeld.
  # Je kan nu deze fout ook niet maken, omdat je interface om aan te geven wat je waaruit wilt afleiden een dataframe met unieke waarden is
  # Als je een situatie als y = x ** 2 hebt, dan moet de gebruiker een goede initiele waarde meegeven zodat de fit op de juiste oplossing komt.


  # Hier gaan we zo veel mogelijk waarden proberen bij te rekenen per jaar
  # In het demografische voorbeeld, moet je bijvoorbeeld mannen uitrekenen uit oude_mannen en jonge_mannen
  min_year <- min(all_dependencies$year)
  max_year <- max(all_dependencies$year)
  variables_with_no_dependencies <- get_equations_with_no_dependencies(
    endogenous_variables = model$get_endo_names(),
    dependency_structure = dependency_structure
  )

  # We rekenen de waarden bij van oud naar nieuw jaar
  for (year in seq(from = min_year, to = max_year, by = 1)) {
    # We willen alleen de vergelijkingen uitrekenen, waarvoor dat kan
    # Verder zit hier de aanname in dat je een vergelijking kan uitrekenen in 2018, als je dat al in 2017 kon, er staat `<= year` en niet `== year`
    # Dat klopt met hoe je over echte modellen en hun data denkt, als ik bijvoorbeeld een exogeen in 2017 ken, dan ook vast in 2018
    # Maar misschien is `<= year` overbodig en wil je `== year`
    active_equations <- all_dependencies[all_dependencies$source == "calculable" & all_dependencies$year <= year, ]
    active_equations <- active_equations$var
    active_equations <- c(active_equations, variables_with_no_dependencies)

    model$set_eq_status(status = "inactive", pattern = "*")
    model$set_eq_status(status = "active", names = active_equations)
    model$order()

    model$run_eqn(names = active_equations, solve_order = TRUE, period = year)
  }


  # Nu gaan we echt de startwaarde fitten en moeten we dus weer alle overbodige vergelijkingen deactiveren
  active_equations <- all_dependencies[all_dependencies$source == "calculable" & all_dependencies$year == year_derive_start_values, ]
  active_equations <- active_equations$var
  active_equations <- c(active_equations, variables_with_no_dependencies)

  model$set_eq_status(status = "inactive", pattern = "*")
  model$set_eq_status(status = "active", names = active_equations)
  model$order()

  # Zetten van fit parameters en data, standaard Isis dingen
  data <- regts::as_data_frame(data_init, format = "long",
                               name_col = "var", period_col = "year") |>
    mutate(year = as.numeric(year)) |>
    filter(!is.na(value))

  model$set_fit(
    data = data[data$var == observed_variable & data$year == year_derive_start_values, ] |> pivot_wider(names_from = var) |> as.regts(time_column = "year"),
    names = c(observed_variable)
  )
  rms_error <- c(1.0)
  names(rms_error) <- derived_variable
  model$set_rms(rms_error)
  model$set_fit_options(maxiter = fit_options$maxiter, cvgrel = fit_options$cvgrel, mkdcrt = fit_options$mkdcrt)

  # Isis klaagt nog steeds over missende data terwijl de vergelijkingen uitgeschakeld zijn.
  # OPM: RVH: dit kan opgelost worden door waarschuwingen uit te schakelen.:q

  # Dus vullen we alle lege lag data op
  # Dat kan gevaarlijk zijn, je rekent met onzin data, maar daarom moet je goed elke vergelijking die je niet wilt draaien deactiveren
  model_data <- model$get_data()
  for (lag in seq(start_period(model_data), year_derive_start_values)) {
    lag <- as.character(lag)
    model_data[lag, ][is.na(model_data[lag, ])] <- 2.0
  }

  model$set_data(model_data)

  # Hier lossen we dan echt op
  model$solve(period = year_derive_start_values)

  # als laatste halen we de uitgerekende startwaarde op en stoppen hem in onze data
  solved_data <- model$get_data()
  calculated_starting_value <- solved_data[as.character(year_derive_start_values), derived_variable][1]
  starting_values_df <- rbind(starting_values_df, list(variable = derived_variable, year = year_derive_start_values, value = calculated_starting_value))
}

# Er komt bij bond in 2021 nu 80 uit.
# Dat klopt als je met de hand rekent:
# assets = stock + bond = 100 + 80 = 180
# management_fee = 0.05 * assets = 0.05 * 180 = 9
# total_fee = 0.01 * stock + management_fee = 0.01 * 1 + 9 = 10
# management_fee_quote = management_fee / total_fee = 9 / 10 = 0.9
# en 0.9 is precies de waarde die we hebben meegegeven

# Afleiding oplossing
# Bekijk eerst deze twee vergelijkingen:
# (1) management_fee = total_fee - 0.01 * stock
# (2) managenent_fee = management_fee_quote * total_fee.
# Omdat stock en management_fee_quote bekend zijn, kunnen we total_fee berekenen:
# total_fee - 0.01 * stock =  management_fee_quote * total_fee
# total_fee * (1 - management_fee_quote) = 0.01 * stock
# total_fee = 0.01 * stock / (1 - management_fee_quote)
# Nu kunnen we management fee berekenen:
# management_fee = 0.01 * stock / (1 - management_fee_quote) - 0.01 * stock
# management_fee = 0.01 * stock * (1 / (1-management_fee_quote) - 1)
# management_fee = 0.01 * stock * (management_fee_quote / (1 - management_fee_quote))
# En nu weten we assets:
# assets = 20 * management fee
# Dus:
# bond = assets - stock
#      =  0.2 * stock * (management_fee_quote / (1 - management_fee_quote)) - stock
#      = stock * (0.2 * (management_fee_quote / (1 - management_fee_quote)) - 1)
#      = 100 * (0.2 * 0.9 / (1 - 0.9) - 1)
#      = 100 * (0.2 * 9 - 1)
#      = 100 *0.8
#       = 80
print(starting_values_df)
if (!(abs(starting_values_df |> filter(variable == "bond") |> select("value") - 80.0) < 1e-5)) {
  stop("oeps")
}

# TODO als alles af is, stop je het stuk na de input (staat boven aangegeven) in een functie en return je starting_values_df
# Maak binnen die functie als eerste een kopie van het modellen object.
# Dan zet je vervolgens de data uit starting_values_df in de data van je oorspronkelijke modellen object.
# En kun je verder rekenen met een volledige set startwaarden.

# nolint end

cat("data:\n")
print(model$get_data())

cat("ca-waarden:\n")
print(model$get_ca())
