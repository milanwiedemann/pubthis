local function append_inlines(target, value)
  if not value then return end
  if pandoc.utils.type(value) == "Inlines" then
    target:extend(value --[[@as pandoc.Inlines]])
  else
    target:insert(pandoc.Str(pandoc.utils.stringify(value)))
  end
end

local function affiliation_text(affiliation)
  local text = pandoc.Inlines({})
  local function append_field(field)
    if field and pandoc.utils.stringify(field) ~= "" then
      if #text > 0 then
        text:insert(pandoc.Str(","))
        text:insert(pandoc.Space())
      end
      append_inlines(text, field)
    end
  end
  append_field(affiliation.department)
  append_field(affiliation.group)
  append_field(affiliation.name)
  append_field(affiliation.address)
  append_field(affiliation.city)
  append_field(affiliation.region or affiliation.state)
  append_field(affiliation["postal-code"])
  append_field(affiliation.country)
  return text
end

local function author_names(authors, affiliation_numbers)
  local names = pandoc.Inlines({})
  for i, author in ipairs(authors or {}) do
    if i > 1 then
      names:insert(pandoc.Str(","))
      names:insert(pandoc.Space())
    end

    if author.name and author.name.literal then
      append_inlines(names, author.name.literal)
    else
      append_inlines(names, author)
    end

    local numbers = pandoc.Inlines({})
    for _, affiliation in ipairs(author.affiliations or {}) do
      local number = affiliation_numbers[
        pandoc.utils.stringify(affiliation.ref)
      ]
      if number then
        if #numbers > 0 then numbers:insert(pandoc.Str(",")) end
        numbers:insert(pandoc.Str(number))
      end
    end
    if #numbers > 0 then names:insert(pandoc.Superscript(numbers)) end
  end
  return names
end

-- Put authors and affiliations in one title block.
function Meta(meta)
  if FORMAT ~= "docx" then return nil end
  local authors = meta.authors
  local affiliations = meta.affiliations
  if not authors or pandoc.utils.type(authors) ~= "List" then return nil end

  local affiliation_numbers = {}
  for _, affiliation in ipairs(affiliations or {}) do
    affiliation_numbers[pandoc.utils.stringify(affiliation.id)] =
      pandoc.utils.stringify(affiliation.number)
  end

  local combined = author_names(authors, affiliation_numbers)
  for _, affiliation in ipairs(affiliations or {}) do
    local line = pandoc.Inlines({})
    line:insert(pandoc.Superscript(
      {pandoc.Str(pandoc.utils.stringify(affiliation.number))}
    ))
    line:insert(pandoc.Space())
    line:extend(affiliation_text(affiliation))

    combined:insert(pandoc.LineBreak())
    combined:insert(pandoc.Span(
      line,
      pandoc.Attr("", {}, {["custom-style"] = "Affiliation"})
    ))
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
