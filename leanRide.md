# leanRide

A lightweight, script-driven substitute for an R IDE, built out of ordinary
macOS apps (Zed, iTerm2/radian, Chromium or Safari, CotEditor) glued together
by a handful of R functions and small scripts, rather than one monolithic
application. It deliberately does not try to reproduce every RStudio/Positron
feature (see "What's not here" at the end) — the goal is the few pieces that
matter for interactive R work: a script editor, a console, a live help
browser, a live plot viewer, a way to send code from editor to console, and a
substitute for the variables/environment pane.

## Install

R packages, from CRAN:

```r
install.packages(c("httpgd", "later", "DT", "htmltools", "htmlwidgets"))
```

`httpgd` and `later` are required for `iastart()`; `DT`, `htmltools`, and
`htmlwidgets` are required for `ienv()`. If you never call `ienv()` you don't
need the latter three.

Other tools, all optional but assumed by parts of this setup:

- **ungoogled-chromium** — `brew install --cask chromium`. If not installed,
  `iastart()` automatically falls back to Safari (see below); nothing breaks
  either way.
- **radian** — a better R console (history, syntax highlighting) than plain
  `R --interactive`, used as the console in the Zed task below. Install via
  `pip install radian` or `brew install radian`.
- **Zed** — the script editor. Extensions: `ocsmit/zed-r` (R language
  support) and `posit-dev/air` (formatter). R packages: `languageserver`,
  `lintr`.
- **CotEditor** — a secondary lightweight editor used for the send-to-console
  keyboard shortcuts described below (Zed's own editor pane works fine too;
  CotEditor is just what the send scripts were written against).
- **Rectangle** — window manager, optional, useful for arranging the several
  windows this setup opens. `rectangle://execute-action?name=...` URL scheme.

## Setup

1. Add `leanRide.r`'s contents to `~/.Rprofile` (or add a line there that
   `source()`s it from wherever you keep it). If you plan to use `ienv()`
   (see below), add `envExclude <- objects(all.names = TRUE)` as the very
   last line of `~/.Rprofile`, after everything else in it has run.
2. In Zed, add to `settings.json`:
   ```json
   {
     "languages": {
       "R": {
         "language_servers": ["air", "r_language_server"],
         "use_on_type_format": false
       }
     },
     "terminal": { "working_directory": "current_project_directory" }
   }
   ```
   and `.zed/tasks.json` in your project:
   ```json
   [
     { "label": "R console", "command": "radian", "use_new_terminal": true, "reveal": "always" }
   ]
   ```
   and a keybinding in `keymap.json` to spawn it:
   ```json
   [
     { "context": "Workspace", "bindings": { "ctrl-alt-r": ["task::Spawn", { "task_name": "R console" }] } }
   ]
   ```
3. Run `zsh leanRide-cot.sh` once — it installs the two CotEditor
   Script-menu items described below (`Rwatch.sh` and `sendrchunks.sh`)
   into `~/Library/Application Scripts/com.coteditor.CotEditor/`, and
   prints the exact steps for binding each to a keyboard shortcut via
   System Settings > Keyboard > Keyboard Shortcuts > App Shortcuts
   (Menu Title must match the script's filename without `.sh`).
4. At the start of an R session, call `iastart()`.

## `iastart()`

```r
iastart(plot_port = 8892, watch = TRUE, force = FALSE, use_chromium = TRUE)
```

Starts R's HTML help server, opens a Help window and a live `httpgd` Plots
window, starts `httpgd` itself, and (unless `watch = FALSE`) starts the
CotEditor-send file watcher (`rsend_watch()`). Safe to call more than once
per session — a second call is a no-op unless you pass `force = TRUE`.

**Browser choice.** If ungoogled-chromium is installed and `use_chromium =
TRUE` (the default), `iastart()` opens two genuinely bare `--app=` Chromium
windows for Help and Plots — no toolbar, tabs, bookmarks bar, or sidebar,
since each is its own chromeless window rather than a tab — sized to 40% of
screen width by 50% of screen height. Otherwise (Chromium not found, or
`use_chromium = FALSE`) it falls back to a single Safari window with Help and
Plots as *tabs*, sized to 1/3 screen width by 1/2 screen height (Safari has
no bare/app mode compatible with tabs, and this setup deliberately never
touches your global Safari view-option preferences, which is the only way to
trim Safari's own chrome further). `use_chromium = FALSE` exists mainly so
you can exercise the Safari path on demand without uninstalling Chromium.

Whichever browser is used, `?topic` help calls are redirected into the
already-open Help window in place (via `options(browser = ...)`), rather
than opening a new OS-default-browser window every time. This redirect keys
off each R session's own dynamic-help port, so it works correctly even with
several `iastart()` sessions running at once — each session updates only its
own Help window, never another session's. Everything else that calls
`browseURL()` — htmlwidgets, DT/gt tables, `Hmisc::describe()`, `ienv()`'s
own output, anything not a help URL — just opens in the OS default browser
(Safari), exactly as it would with no `iastart()` running at all.

**Gotchas this design works around**, if you're modifying `iastart.r`
yourself:

- Launch the Chromium binary directly
  (`.../Chromium.app/Contents/MacOS/Chromium`) rather than via
  `open -a Chromium --args ...`. Without `-n`, `open` silently drops
  `--args` if Chromium is already running; with `-n`, it forces a new OS
  process that immediately exits once it detects the running instance's
  profile lock, causing a visible open/close "flash." Calling the binary
  directly sidesteps `open`/LaunchServices, while Chromium's own singleton
  logic still correctly forwards window flags to the existing process.
- Give both Chromium windows a single, fresh, disposable
  `--user-data-dir` per session. Chromium persists a per-profile
  "remembered" window placement that silently overrides
  `--window-size`/`--window-position` on later launches — a fresh profile
  has no such history, so the flags actually take effect.
- Pass `stdout = FALSE, stderr = FALSE` (discard), not `NULL`, to the
  `system2()` call that launches Chromium. `NULL` means "inherit the
  parent's file descriptors" in `system2()`, not "discard" — since
  Chromium is a long-lived background process, it would otherwise hold
  onto R's own terminal file descriptors for as long as it kept running,
  which both leaked Chromium's internal log lines into the R console and
  could hang the R/radian prompt (a second process writing to the same
  tty confuses readline's raw-mode input handling).
- Launch the browser windows *before* calling `httpgd::hgd()`, not after.
  A long-lived Chromium process launched after `hgd()` opens its listening
  socket inherits a duplicate file descriptor of it (ordinary Unix
  fork/exec behavior), which keeps the port bound at the OS level even
  after R exits — `httpgd::hgd()` then aborts the *entire* R process (an
  uncaught C++ exception, not a catchable R condition) on the next
  `iastart()` call with "Address already in use." `.port_available()` (an
  `lsof -sTCP:LISTEN` check, not a `socketConnection(server=TRUE, ...)`
  probe — the latter blocks waiting for a client and hangs `iastart()`
  forever) and `.find_free_port()` are a defensive second line of
  protection regardless.
- The Safari fallback needs its own per-session help-window targeting,
  just like Chromium's. Safari is a single OS-wide shared app process, so
  with more than one R session running `iastart(use_chromium = FALSE)` at
  once, a plain `open <url>` has no way to know which of Safari's windows
  a given `?topic` call belongs to, and can land in the wrong session's
  window. `.navigate_safari_help()` searches Safari's windows/tabs for the
  one already on this session's own help-server port and updates it in
  place, exactly mirroring `.navigate_chromium_help()`.
- A dedicated third "Output" window for non-help HTML (so it wouldn't have
  to fall through to Safari) was tried and dropped: Chromium's AppleScript
  bridge refuses to navigate a window to a `file://` URL (what most such
  output is), and `--app=file://...` renders a blank window even as a
  fresh process — Chromium's `--app` mode appears to only really support
  http(s). Not worth the added complexity (a local `httpuv` static-file
  server rewriting `file://` to `http://` would likely work, but wasn't
  worth building for a secondary convenience next to Help/Plots).

## Sending code from an editor to R

Two independent CotEditor Script-menu items, both writing to
`~/.rsend/pending.R`, which `rsend_watch()` (started automatically by
`iastart()`) polls every ~0.3s via the `later` package and `source()`s when
it changes — this works at a plain R console, not just inside an IDE,
because `later`'s callbacks run through R's own polled-events mechanism, the
same plumbing that lets Shiny/`httpgd` serve requests while sitting idle at
the `>` prompt.

Both scripts avoid AppleScript's `write text` into iTerm2 (it silently drops
lines near the end of longer pastes — no bracketed-paste framing, so a fast
character stream can outrun the console) and avoid any
Accessibility-permission keystroke automation. `leanRide-cot.sh` installs
both into CotEditor's Scripts folder in one step (see Setup above); a
CotEditor keyboard shortcut still has to be bound to each by hand afterward
(System Settings > Keyboard > Keyboard Shortcuts > App Shortcuts, Menu Title
matching the script's filename without `.sh` — `leanRide-cot.sh` prints the
exact steps when run).

**`Rwatch.sh`** (bind to Cmd-Return): sends exactly what's selected,
unmodified.
```zsh
#!/bin/zsh
# %%%{CotEditorXInput=Selection}%%%
# %%%{CotEditorXOutput=Discard}%%%
mkdir -p ~/.rsend
cat > ~/.rsend/pending.R
```

**`sendrchunks.sh`** (bind to Cmd-Shift-Return): for `.qmd`/`.Rmd`
documents, reproduces RStudio/Quarto's "Run All Chunks Above." Select from
the top of the document to your cursor (Cmd-Shift-Up does this in one step),
then run this instead of `Rwatch.sh` — it strips everything except
the contents of `` ```{r ...} `` fenced chunks (prose, YAML frontmatter,
inline `` `r ...` `` code, and any non-R fenced chunk like `` ```{python} ``,
`` ```{mermaid} ``, `` ```{dot} `` are all dropped). Quarto's `#|`
chunk-option comment lines are left in place deliberately — they're harmless
ordinary R comments when sourced. It does not look at `eval`/`include` chunk
options at all, unlike RStudio's real "Run All Chunks Above," which skips
`eval=FALSE` chunks — every `{r ...}` chunk in the selection runs regardless.
If the selection contains no R chunk at all, the output is empty; use
`Rwatch.sh` for plain `.R` files instead.

**Deprecated: a "Send to iTerm2" script.** An earlier script wrote code
directly into the iTerm2 window via AppleScript (`tell application "iTerm2"
... write text ...`) instead of going through `~/.rsend/pending.R`. That's
exactly the approach `Rwatch.sh`/`rsend_watch()` replaced, for the reason
above (dropped lines on longer pastes) — if you still have this script
installed or bound to a shortcut, it's superseded and safe to delete or
unbind.

## `ienv()` — a lightweight environment pane

```r
ienv(pos = -1L)
```

No standalone macOS tool cleanly replicates RStudio/Positron's live
Environment pane, so `ienv()` is a substitute: it builds an HTML table of the
objects in a given environment and opens it via the same `browseURL()`
mechanism as any other htmlwidget output (so it lands in Safari, or wherever
your current `options(browser=...)` points).

**`pos`** (default `-1L`, the calling environment — ordinarily your global
environment): passed to `objects()`/`ls()`'s own `pos` argument, so it takes
the same values `ls(pos = ...)` does — an integer position in the search
list (`search()`), or a search-list name as a string, e.g.
`ienv(pos = "package:Hmisc")` to inspect what a package exports.

**Hiding your `.Rprofile`'s own clutter with `envExclude`.** `~/.Rprofile`
typically defines a bunch of helper objects and functions of its own
(everything in `leanRide.r`, for instance), and — since it can `source()` in
other files — potentially plenty more that isn't visible just by reading
`~/.Rprofile` itself. Rather than have `ienv()` try to work out what your
Rprofile defines (a static-parsing approach was tried and dropped — it
can't see what a `source()`d file adds), add one line at the very end of
`~/.Rprofile`, after everything else in it has run:

```r
envExclude <- objects(all.names = TRUE)
```

This snapshots the names of every object present at that point — your own
Rprofile's, and anything any file it `source()`d added — regardless of how
they got there. `ienv()` checks for `envExclude` in the environment it's
listing (`pos`): if present, it excludes everything named in it (plus
`envExclude` itself, so that variable doesn't show up as clutter); if
`envExclude` doesn't exist there, `ienv()` just lists everything. This means
`ienv()` run against a plain package environment (`pos = "package:Hmisc"`,
say) always lists everything in it, since there's no `envExclude` to find
there.

Each row shows an object's class, dimensions or length, size, its `label`
and `units` attributes when present (as used by Hmisc-labelled variables),
and any other non-structural attribute it carries (factor levels, a Date's
format, a POSIXct's tzone, custom attributes) — attributes already reflected
in the Class/Dim columns aren't repeated, a function's source code
(`srcref`/`srcfile`/`wholeSrcref`, present whenever
`options(keep.source=TRUE)`, R's interactive default) is never shown, and
the Label/Units/Other-attributes columns are dropped entirely from the table
when nothing in it actually uses them. The title/caption names the
environment being shown — "Global Environment" for the default `pos = -1L`,
or the literal search-list name otherwise (e.g. `package:stats`).

One implementation note if you're modifying this: `pos = -1L` can't simply
be forwarded to `objects(pos = pos)` from inside `ienv()`'s own body — `-1`
means "the environment that called this function," and evaluated from
inside `ienv()` that would resolve to `ienv()`'s own local frame, not your
console's global environment. `ienv()` special-cases `pos = -1` to
`parent.frame()` (evaluated directly in `ienv()`'s own body, so it means
`ienv()`'s actual caller) before doing anything else with it.

## `cotd` — CotEditor folder-adjacent file opening

Add to `.zshrc`. CotEditor 5.0+'s folder-navigator sidebar only appears when
a file is opened via File > Open (Cmd-O) with a specific file selected —
opening a bare directory (`open -a CotEditor <dir>`) just shows a picker, and
scripting the Open panel itself would need Accessibility-permission
keystroke automation, not used anywhere in this setup. `cotd` opens the
most-recently-modified file directly inside a given directory instead:

```zsh
cotd() {
  local target="$1"
  if [[ -d "$target" ]]; then
    local files=("$target"/*(.om[1]))
    if [[ -z "$files" ]]; then
      echo "cotd: no files directly in $target" >&2
      return 1
    fi
    open -a CotEditor "$files[1]"
  else
    open -a CotEditor "$target"
  fi
}
```
Note: opening a file this way does not bring up CotEditor's sidebar (that
association only forms via the Open-panel-selection path) — this is an
accepted trade-off; Zed remains the tool for when an actual folder sidebar
is wanted.

## What's not here

- **A debugger** (breakpoints, step controls, a call stack). Not built —
  low personal priority. `browser()`/`debug()` at the console still work as
  they always have in plain R.
- **A live variables/objects pane that updates automatically.** `ienv()` is
  an on-demand substitute, not a live-updating panel.
- **A third native pane inside Zed for HTML/help content.** Zed's extension
  API has no webview/arbitrary-HTML rendering capability (checked against
  Zed's own GitHub issues/RFCs as of September 2026) — this is why Help and
  Plots are separate OS windows (Chromium or Safari) rather than docked
  inside Zed the way its terminal panel is. If a true single-window,
  3-pane (editor + console + HTML) setup ever becomes a hard requirement,
  Positron (Posit's VS Code fork, which does support webviews) is the tool
  that already does this natively — at the cost of leaving Zed + CotEditor.
