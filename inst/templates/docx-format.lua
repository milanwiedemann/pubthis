-- Keep several authors on one line.
function Meta(meta)
  if FORMAT ~= "docx" then return nil end
  local authors = meta.author
  if not authors or pandoc.utils.type(authors) ~= "List" or #authors < 2 then
    return nil
  end

  local combined = pandoc.Inlines({})
  for i, author in ipairs(authors) do
    if i > 1 then
      combined:insert(pandoc.Str(","))
      combined:insert(pandoc.Space())
    end
    if pandoc.utils.type(author) == "Inlines" then
      combined:extend(author --[[@as pandoc.Inlines]])
    else
      combined:insert(pandoc.Str(pandoc.utils.stringify(author)))
    end
  end

  meta.author = pandoc.MetaInlines(combined)
  return meta
end


function Header(el)
  if FORMAT == "docx" then
    el.identifier = ""
  end
  return el
end


-- Put figure links on captions, not images.
function Div(div)
  if FORMAT ~= "docx" or not div.identifier:match("^fig%-") then return nil end

  local caption = div.content[#div.content]
  if not caption or caption.t ~= "Para" then return nil end

  local has_image = false
  div:walk({ Image = function(_) has_image = true end })
  if not has_image then return nil end

  local identifier = div.identifier
  div.identifier = ""
  div.content[#div.content] = pandoc.Div(
    {caption},
    pandoc.Attr(identifier)
  )
  return div
end


function Table(tbl)
  if FORMAT ~= "docx" then return nil end

  -- Match only single-cell figure tables.
  if not tbl.bodies or #tbl.bodies ~= 1 then return nil end
  local body = tbl.bodies[1]
  if not body.body or #body.body ~= 1 then return nil end
  if body.head and #body.head > 0 then return nil end
  local row = body.body[1]
  if not row.cells or #row.cells ~= 1 then return nil end
  if tbl.head and #tbl.head.rows > 0 then return nil end
  if tbl.foot and #tbl.foot.rows > 0 then return nil end
  if tbl.caption and tbl.caption.long and #tbl.caption.long > 0 then return nil end

  local cell = row.cells[1]
  if not cell.contents then return nil end

  local cell_div = pandoc.Div(cell.contents)
  local has_image = false
  cell_div:walk({ Image = function(_) has_image = true end })
  if not has_image then return nil end

  local unwrapped = cell_div:walk({
    Image = function(img)
      img.attributes["width"]  = nil
      img.attributes["height"] = nil
      return img
    end
  }).content

  return unwrapped
end


-- Handle figures that are not wrapped in tables.
function Figure(fig)
  if FORMAT ~= "docx" then return nil end

  local blocks = {}

  for _, block in ipairs(fig.content) do
    if block.t == "Plain" then
      block = pandoc.Para(block.content)
    end
    block = block:walk({
      Image = function(img)
        img.attributes["width"]  = nil
        img.attributes["height"] = nil
        return img
      end
    })
    table.insert(blocks, block)
  end

  local caption_inlines = pandoc.utils.blocks_to_inlines(fig.caption.long)
  if #caption_inlines > 0 then
    table.insert(blocks, pandoc.Div(
      {pandoc.Para(caption_inlines)},
      pandoc.Attr("", {}, {["custom-style"] = "Caption"})
    ))
  end

  return blocks
end
