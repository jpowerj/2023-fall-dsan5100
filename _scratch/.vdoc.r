# nolint start
#
#
#| label: kroki-api-diagram
# The source code for the mermaid diagram -__-

if (!file.exists("images/random-walk.svg")) {
  mm_source <- readLines("assets/random-walk.mermaid")
  kroki_args <- list(
    #diagram_source = "digraph G {Hello->World}",
    diagram_source = mm_source,
    #diagram_type="graphviz",
    diagram_type="mermaid",
    output_format="svg"
  )
  library(httr2)
  req <- request("https://kroki.io/mermaid/svg/") |>
    req_body_json(kroki_args)
  resp <- req_perform(req)
  svg_str <- resp |> resp_body_string()
  # And save to images/random-walk.svg
  file.create("images/random-walk.svg")
  writeLines(svg_str, con="images/random-walk.svg")
}
# If needed
# https://stackoverflow.com/questions/25166624/insert-picture-table-in-r-markdown/49856416#49856416
#knitr::include_graphics("images/random-walk.svg")
#
#
#
