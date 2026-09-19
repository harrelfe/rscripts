# leanRide

A lightweight, script-driven substitute for an R IDE, built out of ordinary
macOS apps (Zed, iTerm2/radian, Chromium or Safari, CotEditor) glued together
by a handful of R functions and small scripts, rather than one monolithic
application. It deliberately does not try to reproduce every RStudio/Positron
feature (see "What's not here" at the end) — the goal is the few pieces that
matter for interactive R work: a script editor, a console, a live help
browser, a live plot viewer, a way to send code from editor to console, a
substitute for the variables/environment pane, and an interactive data
viewer.

## Install

R packages, from CRAN:

```r
install.packages(c("httpgd", "later", "DT", "htmltools", "htmlwidgets"))
```

`httpgd` and `later` are required for `leanRide()`; `DT`, `htmltools`, and
`htmlwidgets` are required for `vObjects()` and `vData()`. If you never call
either of those you don't need the latter three.

Other tools, all optional but assumed by parts of this setup:

- **ungoogled-chromium** — `brew install --cask chromium`. Used by default
  (see below); if not installed, `leanRide()` automatically falls back to
  Safari and nothing breaks either way.
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
   `source()`s it from wherever you keep it). Nothing else to do for
   `vObjects()` here — `leanRide()` snapshots pre-existing objects itself by
   default (see `epobj` below).
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
4. At the start of an R session, call `leanRide()`.

## `leanRide()`

```r
leanRide(plot_port = 8892, watch = TRUE, force = FALSE, use_chromium = TRUE, epobj = TRUE)
```

Starts R's HTML help server, opens a Help window and a live `httpgd` Plots
window, starts `httpgd` itself, and (unless `watch = FALSE`) starts the
CotEditor-send file watcher (`rsend_watch()`). Safe to call more than once
per session — a second call is a no-op unless you pass `force = TRUE`.

**`epobj`** ("exclude pre-existing objects", default `TRUE`): on its first
call this session, `leanRide()` snapshots every object currently in your
global environment — `~/.Rprofile`'s own objects (everything in
`leanRide.r`, for instance) and anything else present before you started
your actual analysis, including whatever any `source()`d file added — and
stores it as `envExclude` in the global environment, for `vObjects()` to hide
(see below). This replaces having to add that snapshot line to
`~/.Rprofile` yourself. Set `epobj = FALSE` to skip this — useful if you
don't use `vObjects()`, or want to manage `envExclude` yourself. The snapshot is
only taken once per session even across a later `leanRide(force = TRUE)`
call, so re-running `leanRide()` (say, to reopen its windows) never
retroactively hides objects your analysis has already created by then.

**Browser choice.** If ungoogled-chromium is installed and `use_chromium =
TRUE` (the default), `leanRide()` opens two genuinely bare `--app=` Chromium
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
several `leanRide()` sessions running at once — each session updates only its
own Help window, never another session's. Everything else that calls
`browseURL()` — htmlwidgets, DT/gt tables, `Hmisc::describe()`, `vObjects()`'s
own output, anything not a help URL — just opens in the OS default browser
(Safari), exactly as it would with no `leanRide()` running at all.

**Gotchas this design works around**, if you're modifying `leanRide.r`
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
  `leanRide()` call with "Address already in use." `.port_available()` (an
  `lsof -sTCP:LISTEN` check, not a `socketConnection(server=TRUE, ...)`
  probe — the latter blocks waiting for a client and hangs `leanRide()`
  forever) and `.find_free_port()` are a defensive second line of
  protection regardless.
- The Safari fallback needs its own per-session help-window targeting,
  just like Chromium's. Safari is a single OS-wide shared app process, so
  with more than one R session running `leanRide(use_chromium = FALSE)` at
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
`leanRide()`) polls every ~0.3s via the `later` package and `source()`s when
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

## `vObjects()` — a lightweight environment pane

```r
vObjects(pos = -1L)
```

No standalone macOS tool cleanly replicates RStudio/Positron's live
Environment pane, so `vObjects()` is a substitute: it builds an HTML table of the
objects in a given environment and opens it via the same `browseURL()`
mechanism as any other htmlwidget output (so it lands in Safari, or wherever
your current `options(browser=...)` points).

**`pos`** (default `-1L`, the calling environment — ordinarily your global
environment): passed to `objects()`/`ls()`'s own `pos` argument, so it takes
the same values `ls(pos = ...)` does — an integer position in the search
list (`search()`), or a search-list name as a string, e.g.
`vObjects(pos = "package:Hmisc")` to inspect what a package exports.

**Hiding your `.Rprofile`'s own clutter with `envExclude`.** `~/.Rprofile`
typically defines a bunch of helper objects and functions of its own
(everything in `leanRide.r`, for instance), and — since it can `source()` in
other files — potentially plenty more that isn't visible just by reading
`~/.Rprofile` itself. Rather than have `vObjects()` try to work out what your
Rprofile defines (a static-parsing approach was tried and dropped — it
can't see what a `source()`d file adds), `leanRide()` snapshots the names of
every object present in your global environment at the point it first runs
— your own Rprofile's, and anything any file it `source()`d added,
regardless of how they got there — and stores that snapshot as
`envExclude` (see `epobj` above; set `epobj = FALSE` to skip this).
`vObjects()` checks for `envExclude` in the environment it's listing (`pos`): if
present, it excludes everything named in it (plus `envExclude` itself, so
that variable doesn't show up as clutter); if `envExclude` doesn't exist
there, `vObjects()` just lists everything. This means `vObjects()` run against a
plain package environment (`pos = "package:Hmisc"`, say) always lists
everything in it, since there's no `envExclude` to find there. If you'd
rather manage `envExclude` yourself — for instance to also exclude some
objects your analysis creates early on — set `epobj = FALSE` and assign
`envExclude` yourself before relying on `vObjects()`.

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
be forwarded to `objects(pos = pos)` from inside `vObjects()`'s own body — `-1`
means "the environment that called this function," and evaluated from
inside `vObjects()` that would resolve to `vObjects()`'s own local frame, not your
console's global environment. `vObjects()` special-cases `pos = -1` to
`parent.frame()` (evaluated directly in `vObjects()`'s own body, so it means
`vObjects()`'s actual caller) before doing anything else with it.

## `vData()` — an interactive data viewer

```r
vData(x)
```

A substitute for RStudio/Positron's clickable data viewer tab. `vData()`
takes a data frame, data.table, or two-dimensional array (matrix) and opens
it as a sortable, searchable, paginated HTML table via the same
`DT`/`browseURL()` mechanism `vObjects()` uses — so it lands in Safari, or
wherever your current `options(browser=...)` points. Click a column header
to sort by it, and use the search box to filter rows by any text they
contain. Wide tables scroll horizontally rather than compressing columns
unreadably.

Per-column filter widgets (`DT::datatable(..., filter = "top")`) are
deliberately not used: they pull in extra JS libraries (Selectize for
factor/character columns, ionRangeSlider for numeric ranges) that don't
reliably bundle into the selfcontained HTML file `vData()`/`vObjects()`
write and open via `file://` with no web server — when they fail to load,
the whole table breaks (`DataTables warning ... Requested unknown
parameter` is the symptom). The single global search box has no such
dependency and works reliably the same way.

Before display, each column is stripped down to a plain type DT already
knows how to render (`factor`/`ordered`, `Date`, `POSIXct`, `difftime`, or
the bare numeric/integer/character/logical storage mode), and any `label`
or `units` attribute is dropped. This matters for Hmisc-labelled data:
`label()`/`units()` add a `"labelled"` class on top of the real type, and
left in place it can likewise confuse DT's column-type detection and
corrupt the table the same way `filter = "top"` did — same symptom, same
underlying cause (something DT doesn't recognize breaking its column
bookkeeping). Labels and units aren't lost, just not shown here; `vObjects()`
already shows them in its own dedicated columns.

`x` also works as a plain vector, for convenience — named vectors are shown
as two columns (`Name`, `Value`); unnamed ones as a single `Value` column.
Anything with more than two dimensions (a 3-D+ array) isn't supported and
raises an error, since there's no natural 2-D table to show for it.

The window/tab title and table caption both name the argument as you
typed it (`deparse(substitute(x))`), e.g. `Data (mydata)`.

**A data dictionary, as an alternative to `vData()`.** `vData()` shows raw
values, not variable-level metadata (labels, units, value ranges, missing
counts). For an HTML data dictionary instead, set `options(prType = 'html')`
once, then run Hmisc's `contents()` on a data frame or data.table:

```r
options(prType = 'html')
contents(mydata)
```

For substantially more detail per variable (distributions, counts, extremes),
use Hmisc's `describe()` the same way:

```r
describe(mydata)
```

Both open in the browser the same way `vData()`/`vObjects()` do.

## `vPackages()` — CRAN update check

```r
vPackages(update = FALSE)
```

Prints installed vs. CRAN-available versions for any package with an update
pending, using base R's own `old.packages()` — no substitute for RStudio
here, just a short, memorable name for something you'd otherwise type out
by hand. Pass `update = TRUE` to also install everything listed.

Both the check and the install are pinned to `.libPaths()[1]` — your own
default writable library — rather than left to scan/write across every
library on `.libPaths()`. This matters because R's "recommended" packages
(`MASS`, `Matrix`, `survival`, `nlme`, `nnet`, `spatial`, `class`,
`cluster`, `KernSmooth`, `lattice`, and similar) ship bundled with R itself
in a separate system library. Left unpinned, a package present in more than
one library shows up as a *separate row per library* — you may see the same
package name twice, at two different installed versions — and since
`install.packages()` only ever writes to `.libPaths()[1]`, it can never
touch a copy that lives in another library path. That row then reports as
needing an update forever, no matter how many times you run
`vPackages(update = TRUE)`, because the copy it's actually complaining
about never gets touched. Pinning both the check and the install to the
same single library sidesteps this: it always shows and updates only the
packages you actually manage yourself, and leaves R's own bundled
recommended packages alone (their versions track R's own release, not ad
hoc CRAN installs). If you want to see what's installed elsewhere on
`.libPaths()`, run `.libPaths()` to see the list and call `old.packages(lib.loc
= <that path>)` directly.

`installed.packages(lib.loc = lib, noCache = TRUE)` is used rather than
plain `installed.packages()`. Without `noCache = TRUE`,
`installed.packages()`'s own session cache can still report a package's
pre-update version on the very next `old.packages()` call right after
`install.packages()` just updated it — so calling `vPackages()` again after
`vPackages(update = TRUE)` would otherwise list the same packages as still
needing an update, even though they no longer do.

**A package can still show up again even after both fixes above.** CRAN's
*source* package index bumps a package's listed version the moment a
maintainer submits it, but the macOS *binary* build can lag behind that by
anywhere from hours to a day or two. `old.packages()`'s "ReposVer" reflects
the source index, while `install.packages()` (correctly) installs whatever
binary actually exists yet — so if no matching binary has been built, the
"update" silently reinstalls the version you already had, and the package
keeps reappearing here through no fault of `vPackages()` itself. The
recurring `Some listed binary packages have no source` warning from
`install.packages()`/`old.packages()` is CRAN telling you exactly this.
`vPackages(update = TRUE)` checks for this directly — after installing, it
compares each package's version before and after, and for anything that
didn't actually change it prints `vPackages: no newer macOS binary yet on
CRAN for: <names> -- still at the version you had; try again later.`,
rather than leaving you to wonder whether the tool is broken. There's
nothing to do for those but wait and re-run `vPackages(update = TRUE)`
again later.

(One implementation note if you're modifying this: `old.packages()` returns
a normal character matrix when several packages are listed, but a "list
matrix" — `typeof()` `"list"`, not `"character"` — when it finds exactly
one. `vPackages()` rebuilds it as a genuine character matrix first so the
rest of the function behaves the same either way.)

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

## Using Claude Code alongside leanRide

Claude Code can act as a coding assistant for the `.r`/`.qmd` file you have
open in CotEditor (or Zed), without leanRide needing to change at all: it
just edits the file on disk with its ordinary file-editing tools, the same
as if you'd typed the change yourself. CotEditor watches its open documents
and reloads them automatically when they change externally, so an edit
Claude Code makes shows up in the open buffer right away.

The one thing to watch for is save order. If you have unsaved edits of your
own sitting in the buffer when Claude Code writes to the file, CotEditor
will flag the conflict rather than silently discarding one side — so get in
the habit of saving (Cmd-S) before asking Claude Code to touch a file you've
been typing in, both so it's editing the version you actually meant and so
you don't lose your own in-progress changes.

Execution stays entirely manual and separate from this: Claude Code never
touches `~/.rsend/pending.R` or iTerm2 in this workflow. You review what it
changed, select what you want to run, and trigger it yourself with
`Rwatch.sh` or `sendrchunks.sh` as always — then report back anything
worth knowing (an error, unexpected output, a plot that looks wrong) for
the next round.

If you'd rather have Claude Code see what's actually being submitted to R —
either to keep a record of it, or to review a selection before it's run —
it can instead watch `~/.rsend/pending.R` for changes, since that file
already holds exactly what each send script queues up. This is a separate,
optional way of working from the file-editing one above (Claude Code
reading what you send, rather than writing what you'll send) and the two
can be combined if useful.

## What's not here

- **A debugger** (breakpoints, step controls, a call stack). Not built —
  low personal priority. `browser()`/`debug()` at the console still work as
  they always have in plain R.
- **A live variables/objects pane that updates automatically.** `vObjects()` is
  an on-demand substitute, not a live-updating panel.
- **A third native pane inside Zed for HTML/help content.** Zed's extension
  API has no webview/arbitrary-HTML rendering capability (checked against
  Zed's own GitHub issues/RFCs as of September 2026) — this is why Help and
  Plots are separate OS windows (Chromium or Safari) rather than docked
  inside Zed the way its terminal panel is. If a true single-window,
  3-pane (editor + console + HTML) setup ever becomes a hard requirement,
  Positron (Posit's VS Code fork, which does support webviews) is the tool
  that already does this natively — at the cost of leaving Zed + CotEditor.
