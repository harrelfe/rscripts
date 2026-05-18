--[[
wrap_turns.lua — Pandoc Lua filter
Wraps each chat turn (h2 speaker heading + following content up to the
next hr or h2) in a <div class="turn frank|claude"> block.

Structure in the flat AST:
  HorizontalRule
  Header 2 "Frank"   ← speaker
  Para ...           ← content
  HorizontalRule
  Header 2 "Claude"  ← speaker
  Para / BulletList / CodeBlock / Table ...
  HorizontalRule
  ...
]]

function Pandoc(doc)
  local blocks = doc.blocks
  local out    = {}
  local i      = 1
  local n      = #blocks

  while i <= n do
    local blk = blocks[i]

    -- Detect a speaker heading: level-2 header whose text is Frank or Claude
    if blk.t == "Header" and blk.level == 2 then
      local text = pandoc.utils.stringify(blk)
      local speaker = text:match("^(Frank)") or text:match("^(Claude)")

      if speaker then
        local cls = speaker:lower()  -- "frank" or "claude"

        -- Build the label span (replaces the bare h2)
        local label = pandoc.Div(
          { pandoc.Para({ pandoc.Str(text) }) },
          pandoc.Attr("", {"turn-label"})
        )

        -- Collect content blocks until the next hr or level-2 header
        local content = {}
        i = i + 1
        while i <= n do
          local cb = blocks[i]
          if cb.t == "HorizontalRule" then
            break  -- end of this turn (consume the hr below)
          elseif cb.t == "Header" and cb.level == 2 then
            break  -- next speaker starts (don't consume)
          else
            content[#content + 1] = cb
            i = i + 1
          end
        end

        -- Wrap label + content in div.turn.(frank|claude)
        local bubble = pandoc.Div(content, pandoc.Attr("", {"bubble"}))
        local turn   = pandoc.Div({label, bubble},
                         pandoc.Attr("", {"turn", cls}))
        out[#out + 1] = turn

        -- Skip the trailing hr if present
        if i <= n and blocks[i].t == "HorizontalRule" then
          i = i + 1
        end

      else
        -- Non-speaker h2: pass through unchanged
        out[#out + 1] = blk
        i = i + 1
      end

    else
      -- Everything before the first speaker (title para, leading hr, etc.)
      out[#out + 1] = blk
      i = i + 1
    end
  end

  return pandoc.Pandoc(out, doc.meta)
end
