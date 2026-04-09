# Documentatie van Aanpassingen (`fix-build-and-stability` branch)

Dit document beschrijft de wijzigingen die zijn aangebracht in de branch `fix-build-and-stability` ten opzichte van de `master` branch. Het hoofddoel van deze aanpassingen was het verhelpen van linker-errors, het verbeteren van de stabiliteit van de code en het verfijnen van de foutafhandeling.

## Overzicht van Wijzigingen

### 1. Build Systeem & Linker Fixes
* **Bestand**: `pkg/src/deps/makedeps_f90`
* **Aanpassing**: De dependency `output_utils.o` is toegevoegd aan de build-target voor `mdgfbo.o`.
* **Reden**: Voorheen mislukte de compilatie/link fase omdat `mdgfbo.f90` functies uit de `output_utils` module aanriep zonder dat deze correct gekoppeld waren in het Makefile-systeem.

### 2. C-Code Stabiliteit & Geheugenbeheer
* **Bestand**: `pkg/src/c_calls.c`
* **Foutafhandeling**: In de functie `write_mdl_c` is de algemene "Unknown error" melding vervangen door specifieke checks op foutcodes (bijv. `ier == 1` voor bestandsfouten en `ier == 4` voor geheugen tekort).
* **Geheugenlek / Crash Fix**: In `get_param_c` is een cruciale correctie doorgevoerd in de `PROTECT`/`UNPROTECT` stack. Voorheen was er een risico op een stack imbalance tijdens het ophalen van parameters in een loop. Dit is nu genormaliseerd zodat elk element correct wordt beschermd en vrijgegeven binnen de loop-structuur.

### 3. Fortran Veiligheid (Defensive Programming)
* **Bestand**: `pkg/src/mcfbor.f90`
* **Bounds Checking**: Er zijn expliciete controles toegevoegd om te verifiëren of array-indices binnen de toegestane grenzen vallen (`1` tot `mdl%nrv` of `mdl%neq`). 
* **Reden**: Dit voorkomt "Segmentation Faults" (crashes) die ontstaan wanneer de solver buiten het gereserveerde geheugen probeert te schrijven. Indien een index buiten bereik is, wordt nu netjes foutcode `4` teruggegeven.

### 4. Verbeterde Foutrapportage in Fortran
* **Bestand**: `pkg/src/mdgfbo.f90`
* **Messaging**: De module `output_utils` wordt nu gebruikt om via `isismdl_out` directe, leesbare foutmeldingen naar de R-console te sturen (bijv. specifiek over geheugentekort of array-overschrijdingen in `gen_fb_order`).

### 5. Metadata & Project Opschoning
* **Bestand**: `pkg/DESCRIPTION`
* **Aanpassing**: Dubbele imports van het `rlang` package zijn verwijderd.
* **Reden**: Voorkomt waarschuwingen tijdens R CMD check en houdt de projectstructuur schoon.

### 6. Nieuwe Tools & Documentatie
* **BUGHUNTING.md**: Een nieuw rapport dat potentiële risicogebieden voor instabiliteit in kaart brengt.
* **test_dir.R**: Een handig script om de installed versie van het package te testen tegen de testsuite.
* **lints.txt / check_out.txt**: Logbestanden van statische analyse en build-tests om voortgang te bewaken.

---

## Verificatie
De bovenstaande wijzigingen zijn geverifieerd door:
1. Een succesvolle build zonder linker-errors.
2. Handmatige controle van de loop-logica in de C-code.
3. Het draaien van de testsuite via `test_dir.R`.
