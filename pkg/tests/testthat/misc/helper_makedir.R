cat("Helper\n")
if (!dir.exists("output")) {
  # We have to make sure that directory output is present. If the test is run with
  # Travis CI, then directory output is missing even though output/.gitignore is
  # part of the repo.
  dir.create("output")
}
