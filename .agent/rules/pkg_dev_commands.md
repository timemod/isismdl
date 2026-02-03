
# R Package Development Rules

You are working in an R package project (it contains `DESCRIPTION`, `NAMESPACE`, `R/`, etc.).  
Your job is to automatically run the appropriate `R CMD` or `devtools` commands for common package‑development tasks, **without me having to type them manually each time**.

## General Behavior

- Always assume the **project root directory** is the folder containing the `DESCRIPTION` file.
- When I mention “build”, “check”, “test”, or “document” the package, you should:
  1. Open or use an existing terminal in the project root.
  2. Run the corresponding commands listed below.
- Prefer **`devtools`** when available; otherwise fall back to `R CMD` commands.

## Commands to Use

### 1. Document the package (roxygen)

When I say things like:

- “document the package”
- “document pkg”
- “run roxygen”
- “update documentation”
- “re‑generate docs / NAMESPACE”

You should run:

```bash
R -q -e "devtools::document()"
```

If `devtools` is not installed or not available, you may run:

```bash
R -q -e "roxygen2::roxygenise()"
```

### 2. Load All (like RStudio’s Cmd+Shift+L)

When I say:

- “load the package”
- “load all”
- “reload the package in the session”

You should run:

```bash
R -q -e "devtools::load_all()"
```

### 3. Build the package

When I say:

- “build the package”
- “build pkg”
- “create a source tarball”
- “build for CRAN”

You should run **either**:

```bash
R -q -e "devtools::build()"
```

or, equivalently, using base R:

```bash
R CMD build .
```

### 4. Install (build + install locally)

When I say:

- “install the package”
- “install pkg”
- “build and install”
- “reinstall the package”

You should run:

```bash
R -q -e "devtools::install()"
```

If using base R only, then:

1. Build:
   ```bash
   R CMD build .
   ```
2. Install the resulting tarball (replace with actual version):
   ```bash
   R CMD INSTALL mypackage_0.1.0.tar.gz
   ```

### 5. Test the package

When I say:

- “run tests”
- “test the package”
- “test pkg”
- “execute testthat tests”

You should run:

```bash
R -q -e "devtools::test()"
```

If I explicitly ask to use `R CMD check`‑style tests only, you may instead rely on:

```bash
R CMD check mypackage_0.1.0.tar.gz
```

after a build, but by default prefer `devtools::test()`.

### 6. Check the package

When I say:

- “check the package”
- “check pkg”
- “run R CMD check”
- “run a check”
- “do a full package check” or “CRAN check”

You should run:

```bash
R -q -e "devtools::check()"
```

If `devtools` is not available, then:

1. Build:
   ```bash
   R CMD build .
   ```
2. Check:
   ```bash
   R CMD check mypackage_0.1.0.tar.gz
   ```

## Error Handling

- If a command fails, show me:
  - The exact command you ran.
  - The **error output** from the terminal.
- Do **not** silently retry with different commands unless I ask; instead, ask me how to proceed (for example, whether to switch from `devtools::check()` to `R CMD check`, or vice versa).

## Summary of Mappings

- **“Document” / “roxygenise”** → `R -q -e "devtools::document()"`
- **“Load all” / “reload”** → `R -q -e "devtools::load_all()"`
- **“Build”** → `R -q -e "devtools::build()"` (or `R CMD build .`)
- **“Install”** → `R -q -e "devtools::install()"`
- **“Test”** → `R -q -e "devtools::test()"`
- **“Check” / “R CMD check”** → `R -q -e "devtools::check()"` (or `R CMD build .` then `R CMD check <tarball>`)

Always prefer these automated commands whenever I ask you to build, check, or test the package.