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
# chunk-boundary parsing or Accessibility-permission UI automation.
#
# All the actual chunk/inline-R extraction logic lives in ~/bin/rchunks
# (shared with Zed -- see leanRide.md), so this script is just glue: hand
# CotEditor's selection (stdin) to rchunks and write its output to
# pending.R for rsend_watch() to pick up. See ~/bin/rchunks for exactly
# what gets extracted and why.
#
# Uses rchunks's full path rather than relying on PATH, since CotEditor
# runs this script as a plain child process that does not source your
# ~/.zshrc or ~/.zprofile -- so a bare `rchunks` call could fail with
# "command not found" even though it works fine typed into iTerm2.

mkdir -p ~/.rsend
~/bin/rchunks > ~/.rsend/pending.R
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
