mkdir -p ~/"Library/Application Scripts/com.coteditor.CotEditor"

cat > ~/"Library/Application Scripts/com.coteditor.CotEditor/Rwatch.sh" <<'EOF'
#!/bin/zsh
# %%%{CotEditorXInput=Selection}%%%
# %%%{CotEditorXOutput=Discard}%%%
mkdir -p ~/.rsend
cat > ~/.rsend/pending.R
EOF
chmod +x ~/"Library/Application Scripts/com.coteditor.CotEditor/Rwatch.sh"

cat > ~/"Library/Application Scripts/com.coteditor.CotEditor/sendrchunks.sh" <<'EOF'
#!/bin/zsh
# %%%{CotEditorXInput=Selection}%%%
# %%%{CotEditorXOutput=Discard}%%%
#
# Bind to Cmd-Shift-Return (System Settings > Keyboard > Keyboard Shortcuts >
# App Shortcuts, matching this script's menu title exactly), alongside the
# existing Cmd-Return "send selection as-is" script.
#
# Companion to the plain send-selection script, for .qmd/.Rmd documents.
# Workflow: click at (or inside) the chunk you're about to run, press
# Cmd-Shift-Up to select from there to the top of the document, then this
# shortcut -- reproduces RStudio/Quarto's "Run All Chunks Above" without any
# chunk-boundary parsing or Accessibility-permission UI automation: we only
# ever look at the text CotEditor hands us on stdin (the selection).
#
# Strips everything except the contents of ```{r ...} fenced chunks:
# prose, YAML frontmatter, inline `r ...` code, and any non-R fenced chunk
# (```{python}, ```{mermaid}, ```{dot}, etc.) are all dropped. Quarto's
# #| chunk-option comment lines are deliberately NOT stripped out of R
# chunks -- they're harmless ordinary R comments when sourced, not worth
# the extra complexity of parsing them out. Likewise this doesn't look at
# eval/include chunk options at all: every {r ...} chunk in the selection
# runs, regardless of eval=FALSE -- unlike RStudio's own "Run All Chunks
# Above", which does skip eval=FALSE chunks. Revisit if that distinction
# ever actually matters in practice.
#
# If the selection contains no ```{r ...} fence at all (e.g. run by
# habit on a plain .R file, or a .qmd selection with no R chunks in range),
# the output is empty -- there's nothing to extract. Use the plain
# Cmd-Return script for that case instead.

mkdir -p ~/.rsend
awk '
  {
    line = $0
    if (line ~ /^[[:space:]]*```/) {
      if (in_r) {
        in_r = 0
      } else if (line ~ /^[[:space:]]*```\{r[ ,}]/) {
        in_r = 1
      }
      next
    }
    if (in_r) print line
  }
' > ~/.rsend/pending.R
EOF
chmod +x ~/"Library/Application Scripts/com.coteditor.CotEditor/sendrchunks.sh"

echo "System Settings -> Keyboard (in the sidebar) -> Keyboard Shortcuts
This opens a separate window with its own sidebar of categories; select App Shortcuts near the bottom.
Click the + button below the list.  A sheet drops down with three fields:
Application: click the pop-up and pick CotEditor which narrows the scope of the new shortcuts.
Menu Title: type the script's menu name exactly, either Rwatch or sendrchunks (withOUT the .sh).
Keyboard Shortcut: click into that field and just press the actual key combination you want
(e.g. hold cmd and press return for Rwatch; hold Shift-Cmd-return for sendrchunks) —
don't type it as text.
Click Add (or Done), then close the window.\n"
