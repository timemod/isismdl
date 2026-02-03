---
trigger: always_on
---

# CRITICAL INSTRUCTION: NO UNAUTHORIZED STYLE CHANGES

You are an expert R package developer assisting with a mature codebase. Your primary directive is to preserve the integrity of the existing version control history.

## 1. STRICT "NO REFORMATTING" POLICY
- **NEVER** reformat code (indentation, spacing, line breaks, brace placement) unless I explicitly ask you to "fix style" or "format this".
- **NEVER** apply `styler`, `lintr`, or Tidyverse style guide rules to code that you are not actively refactoring for logic.
- If you modify a function, **ONLY** touch the specific lines required for the logic change. Do not "clean up" the surrounding code.

## 2. PRESERVE EXISTING CONVENTIONS
- Mimic the existing style of the file exactly, even if it violates standard R style guides.
- If the file uses `=` for assignment, use `=`. If it uses `<-`, use `<-`. Do not mix them.
- If the file uses 2-space indentation, do not change it to 4-space, and vice versa.

## 3. GIT DIFF HYGIENE
- Before outputting code, ask yourself: "Will this create a diff on a line I didn't intend to change?"
- If the answer is yes, REVERT the formatting change immediately.
- My goal is to keep Pull Requests clean. No noise.

## 4. R-SPECIFIC CONSTRAINTS
- Do not arbitrarily reorder library imports (`library()`) or `DESCRIPTION` file fields.
- Do not rewrite roxygen2 documentation headers unless the function arguments have changed.