---
trigger: always_on
---

# Working Directory Management Rules

You are assisting in an R (or general coding) project.  
Your job is to **quickly switch the working directory** between:

1. The **project root directory** (the main folder), and  
2. The **directory of the currently open file** in the editor.

This should work both for **shell/terminal commands** and for **R sessions** when appropriate.

---

## Definitions

- **Project root / main folder**:  
  The folder that contains the main project files (for an R package, this is the folder with `DESCRIPTION`, `NAMESPACE`, `R/`, etc.).  
  Assume Antigravity’s project root is this folder.

- **Current file directory**:  
  The folder that contains the file currently open and active in the editor.

Whenever I ask you to “set the working directory” or “switch directory”, you should interpret it in one of these two ways and apply the rules below.

---

## General Behavior

- Always clarify whether the change applies to:
  - The **terminal shell** (e.g. `cd`), or  
  - The **R working directory** (e.g. `setwd()`), or both.
- Prefer changing the **current terminal’s working directory** when I’m running command‑line tools.
- When I’m clearly working in R (e.g. running `devtools::check()` or `source()`), you may also offer to update the working directory *inside R*.

---

## 1. Set working directory to project root

Trigger phrases include (examples):

- “go to project root”
- “switch to main folder”
- “set wd to root”
- “set wd root”
- “set root wd”
- “cd to package root”
- “switch back to the main directory”
- “reset working directory”

### In the terminal (shell)

When I use any of the phrases above and I’m in a terminal, you should run:

```bash
cd /path/to/project/root
```

(Use the actual Antigravity project root path or the folder that contains the `DESCRIPTION` file.)

If you already know the project root (e.g. `${PROJECT_ROOT}`), you can use:

```bash
cd "${PROJECT_ROOT}"
```

### In R

If I am clearly in an R context and indicate I want the R working directory to be the project root, you should run:

```r
setwd("/path/to/project/root")
```

or, if Antigravity exposes a project root variable, use that instead of a hard‑coded path.

---

## 2. Set working directory to the current file’s directory

Trigger phrases include (examples):

- “set wd to current file”
- “set wd current”
- “set current wd”
- “set wd curr”
- “set curr wd”
- “wd to open file”
- “use the folder of this file”
- “switch to the directory of the open file”
- “cd to this file’s directory”
- “run from the current file’s folder”

### In the terminal (shell)

When I use any of the phrases above, you should:

1. Determine the full path of the currently active/open file (call this `CURRENT_FILE`).
2. Change the terminal directory to that file’s directory:

   ```bash
   cd "$(dirname "CURRENT_FILE")"
   ```

   Replace `CURRENT_FILE` with the actual file path Antigravity provides (for example, `/home/user/project/R/my_script.R`).

### In R

If I am clearly in an R context and ask for the working directory to be the current file’s directory, you should:

1. Get the active file’s path (for example, from the editor integration).
2. In R, run:

   ```r
   setwd(dirname("CURRENT_FILE"))
   ```

   Again, replace `"CURRENT_FILE"` with the actual path of the open file.

---

## 3. When I don’t specify which one I mean

If I say something ambiguous like:

- “set the working directory”
- “change directory”
- “go to the right folder first”

Then:

1. **Ask a quick clarification**, e.g.:

   > “Do you want the working directory to be the project root, or the folder of the currently open file?”

2. If there is strong context (for example, I explicitly mentioned “project root” earlier or I’m about to run `devtools::check()`), assume:
   - **Project root** for package‑level tasks (`build`, `check`, `test`, etc.).
   - **Current file directory** for running a single script or file‑specific task (`source()`, reading local data next to the script, etc.).

---

## 4. Keep me informed

Whenever you change the working directory, **always show me**:

- The command you used, and  
- The resulting path, for example:

```bash
cd /home/user/project
pwd
# /home/user/project
```

or, in R:

```r
setwd("/home/user/project/R")
getwd()
# "/home/user/project/R"
```

---

## Summary of Mappings

- **“Project root / main folder”**  
  - Terminal: `cd /path/to/project/root`  
  - R: `setwd("/path/to/project/root")`

- **“Current file directory”**  
  - Terminal: `cd "$(dirname "CURRENT_FILE")"`  
  - R: `setwd(dirname("CURRENT_FILE"))`
