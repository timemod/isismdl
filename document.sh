#!/bin/bash
PKG=isismdl
R=R
echo "roxygen2::update_collate('"$PKG"'); devtools::document('"$PKG"')"
R -e "roxygen2::update_collate('"$PKG"'); devtools::document()"
$R CMD Rd2pdf --force --no-preview -o manual.pdf ../$PKG
