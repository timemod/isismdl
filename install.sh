#!/bin/bash
R=R
PKG=isismdl
$R CMD INSTALL --no-multiarch --with-keep.source ../$PKG
