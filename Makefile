# This is a gnu makefile with several commands to build, document and test
# the package.  The actual building and installation of the package is achieved
# with the standard R commands R CMD BUOLD and R CMD INSTALL.

PKGDIR=pkg
INSTALL_FLAGS=--no-multiarch --with-keep.source
RCHECKARG=--no-multiarch
PKG_FFLAGS=-fimplicit-none -cpp -J $(PKGDIR)/src/mod -I $(PKGDIR)/src/include
PKG_CFLAGS=

# Package name, Version and date from DESCIPTION
PKG=$(shell grep 'Package:' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2)
PKGTAR=$(PKG)_$(shell grep 'Version' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2).tar.gz
PKGDATE=$(shell grep 'Date' $(PKGDIR)/DESCRIPTION  | cut -d " " -f 2)
TODAY=$(shell date "+%Y-%m-%d")

OSTYPE=$(shell Rscript -e "cat(.Platform[['OS.type']])")

help:
	@echo
	@echo "The following targets are available:"
	@echo "   help      - displays this help"
	@echo "   test      - run the tests"
	@echo "   covr      - check package coverage (package covr)"
	@echo "   check     - Run R CMD check $(PKGDIR)"
	@echo "   syntax    - check syntax .f and .c files"
	@echo "   document  - run roxygen to generate Rd files and make pdf Reference manual"
	@echo "   data      - generate data"
	@echo "   mkpkg     - builds source package, add to drat and checks with --as-cran"
	@echo "   bin       - builds binary package in ./tmp"
	@echo "   install   - install package in .libPaths()[1]"
	@echo "   installv  - install package with vignettes in .libPaths()[1]"
	@echo "   uninstall - uninstall package from .libPaths()[1]"
	@echo "   clean     - cleans up everything"
	@echo "   flags     - display R config flags and some macros"

# make sure that R's variables are used
# if you don't do this you'll get make's initial values
# gives error doing syntax target
#R_CPPFLAGS=$(shell R CMD config --cppflags)
FC=$(shell R CMD config FC)
F77=$(shell R CMD config F77)
CC=$(shell R CMD config CC)
CPP=$(shell R CMD config CXX)
CPP_FLAGS=$(shell R CMD config --cppflags)

flags:
	@echo "OSTYPE=$(OSTYPE)"
	@echo "PKGDIR=$(PKGDIR)"
	@echo "PKG=$(PKG)"
	@echo "PKGTAR=$(PKGTAR)"
	@echo "PKGDATE=$(PKGDATE)"
	@echo "FC=$(FC)"
	@echo "F77=$(F77)"
	@echo "CC=$(CC)"
	@echo ".libPaths():"
	@R --no-save --quiet --slave -e '.libPaths()'

test:
	R --slave -f test.R

test_covr:
	R --slave -f  test_covr.R

check: cleanx syntax
	@echo " *** Running R CMD check ***"
	$(MAKE) -f Makedeps
	R CMD build $(PKGDIR)
	R CMD check $(RCHECKARG) $(PKGTAR)
	@rm -f  $(PKGTAR)
	@echo "Today                           : $(TODAY)"
	@echo "Checked package description date: $(PKGDATE)"
# 	@Rscript -e 'cat("Installed version date          :",packageDescription("nleqslv", fields="Date"))'
	@echo ""


syntax: bin
	# To make sure that all Fortran module files (.mod) files are present,
	# we have to run the bin target before we can check the syntax.
	$(FC) $(PKG_FFLAGS) -c -fsyntax-only -Wall -pedantic $(PKGDIR)/src/*.f90
	$(CC) $(CPP_FLAGS) $(PKG_CFLAGS) -std=gnu99 -c -fsyntax-only -Wall -pedantic $(PKGDIR)/src/*.c

cleanx:
# Apple Finder rubbish
ifneq ($(OSTYPE), windows)
	@find . -name '.DS_Store' -delete
endif
	@rm -f $(PKGTAR)
	@rm -fr $(PKG).Rcheck

# build date of package must be at least today
# build source package for submission to CRAN
# after building do a check as CRAN does it
mkpkg: cleanx syntax
ifeq ($(OSTYPE), windows)
	@echo Please run mkpkg on Linux or MAC OSX
else
	R CMD build $(PKGDIR)
	#R CMD check --as-cran $(RCHECKARG) $(PKGTAR)
	@cp -nv $(PKGTAR) archive
	@echo "Today                           : $(TODAY)"
	@echo "Checked package description date: $(PKGDATE)"
	@echo "Checked package description date: $(PKGDATE)"
	@echo ""
	./drat.sh --pkg=$(PKGTAR)
endif

bin: install_deps
	$(MAKE) -f Makedeps
	-@rm -rf tmp
	mkdir tmp
	R CMD build $(PKGDIR)
	R CMD INSTALL $(INSTALL_FLAGS) -l ./tmp --build $(PKGTAR)

document: install_deps
	$(MAKE) -f Makedeps
	-@rm -f isismdl.pdf
	R -e "devtools::document('"$(PKGDIR)"')"
	R CMD Rd2pdf --batch $(PKGDIR) -o isismdl.pdf 2>isismdl.log

install: install_deps
	$(MAKE) -f Makedeps
	R CMD INSTALL $(INSTALL_FLAGS) $(PKGDIR)

installv: install_deps
	$(MAKE) -f Makedeps
	R CMD build $(PKGDIR)
	R CMD INSTALL $(INSTALL_FLAGS) $(PKGTAR)

install_deps:
	R --slave -f install_deps.R

uninstall:
	R CMD REMOVE $(PKG)

clean:
	$(MAKE) -f Makedeps clean
	rm -fr $(PKGDIR).Rcheck
	rm -fr tmp
	rm -f $(PKGTAR)
	rm -f $(PKGDIR).pdf
	rm -f $(PKGDIR).log
	rm -f $(PKGDIR)/src/*.o
	rm -f $(PKGDIR)/src/*.so
	rm -f $(PKGDIR)/src/*.dll
	rm -f $(PKGDIR)/src/mod/*.mod
	rm -f $(PKGDIR)/deps/*.pkl
	rm -f $(PKGDIR)/tests/testthat/islm/mdl/*.mif
	rm -f $(PKGDIR)/tests/testthat/islm/mdl/*.mrf
	rm -f $(PKGDIR)/data-raw/*.mif
	rm -f $(PKGDIR)/data-raw/*.mrf
	rm -f examples/*.mrf
	rm -f examples/*.mif
	rm -f examples/*.err
