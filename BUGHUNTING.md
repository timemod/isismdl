# Bug Hunting Report: `isismdl`

Tijdens het analyseren van het `isismdl` project zijn de volgende operationele obstakels, potentiële bugs en risicofactoren voor instabiliteit (waaronder segmentation faults) vastgesteld.

## 1. Configuratie en Test Bugs
* **Dubbele afhankelijkheid in DESCRIPTION**: In `pkg/DESCRIPTION` wordt het package `rlang` herhaald in de `Imports:` sectie (op zowel regel 24 als 28). Dit is een metadata configuratiefout die waarschuwingen genereert bij R package validatietools en CRAN tests.
* **Foutieve library aanroep in tests**: In `test_dir.R` (regel 5) staat een aanroep naar `library(cbsots)`. Het package `cbsots` is niet lokaal geïnstalleerd, wordt niet genoemd in de project dependency's, en vermoedelijk moet dit `library(isismdl)` zijn om de eigen tests via deze wrapper legitiem te kunnen draaien.

## 2. Instabiliteit en `Segfault` Risico's in C/Fortran Code
Omdat dit project native memory beheert en interageert via de `.Call` interface, dragen de volgende factoren aanzienlijk bij aan mogelijke hardwarematige crashes (segfaults) in R:

### A. Ontbrekende Foutafhandeling (Null Pointers)
Er is gebleken dat meerdere kritieke controlepunten in C en Fortran nog niet zijn afgewerkt. Als een geheugenallocatie of parsing bewerking faalt en de pointer status niet strak gecontroleerd is, zal het systeem dit later dereferencen en crashen. Voorbeelden uit de code:
* **`pkg/src/c_calls.c`**: Op o.a. regel 140 is achtergelaten `// TODO: add correct error handling`.
* **`pkg/src/mdgfbo.f90`**: In deze code voor het model staan meerdere notities over precaire secties `! todo: error handling`.

### B. Onveilige R naar C transities zonder strenge type-controle (Type Mismatches)
In `R/IsisMdl.R` wordt native C-code aangestuurd via tientallen `.Call()` executies, bijv. `C_read_mdl_c`, `C_get_data_c` en `C_solve_c`. 
Als de C wrapper (`c_calls.c`) inkomende R-argumenten opvraagt via macros zoals `INTEGER()`, `REAL()`, en `VECTOR_ELT()`, loopt alles in de soep als het argument onverwacht een pointer, string, of lijst bleek te zijn in plaats van de correcte primitieve data structuur. Strikt testen via `TYPEOF(x)` en coercing aan de R of C grens (zoals met `AS_NUMERIC`) zijn belangrijke verdedigingslinies hiertegen die niet altijd gegarandeerd zijn waardoor out-of-bounds lezen van de memory stack makkelijk voorkomt.

### C. Geen Garbage Collection veiligheid / PROTECT stack fouten
De `c_calls.c` routine bouwt regelmatig complexe dynamische lijsten via R's C API, roept veel `allocVector(STRSXP, ...)` en `allocVector(REALSXP, ...)` aan. 
* Telkens wanneer memory via R gereserveerd is voor een object in C, moet dat veilig afgesloten worden met `PROTECT()` en vrijgegeven met `UNPROTECT()`.
* Een kleine pointer allocatie die kort hierna een nested macro in werking zet maar niet direct (of de elementen daarin) van een deugdelijk `PROTECT()` kader was voorzien, riskeert door R's GC als *"overbodig"* weggestreept te worden nog tijdens de C-executie. Een latere aanroep genereert dan onherroepelijk een crash.
* Zelfs een `stack imbalance in .Call` pre-waarschuwing kan zich ontwikkelen in instabiliteit op langere termijn.

### D. Fortran Array Boundaries
Fortran (`pkg/src/*.f90`) code kan geheugen scherpe indexering verlangen die makkelijk leeg loopt buiten de toegewezen grens (een classic buffer overflow) als parameters uit C (of modellen afkomstig van de R layer) onvoorbereid of in te groot volume op de solver klappen.

## Geadviseerde Vervolgstappen
* **Dubbel dependencies opruimen:** Corrigeer direct `DESCRIPTION` en `test_dir.R` als 'low hanging-fruit'.
* **Valgrind / Address Sanitizer:** Bij aanhoudende segfaults in de praktijk dient een dynamische geheurgen analyse suite te draaien (in linux via `R -d valgrind`) op het moment dat het model draait. Memory leaks en bad access adressen lichten daarmee direct op met regelnummers in `c_calls.c` of `scan.c`.
