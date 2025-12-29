



config <- data.table::rbindlist(
  l = list(
    data.table::data.table(
      path = "Data/Grid/Tab1",
      content = "hi",
      sticon = "file"
    ),
    data.table::data.table(
      path = "Data/Grid/Tab2",
      content = "hi2",
      sticon = "address-book"
    ),
    data.table::data.table(
      path = "Data/Grid/Tab3",
      content = "hi3",
      sticon = "clock"
    ),
    data.table::data.table(
      path = "Data2/Grid2/Tab2",
      content = "ho",
      sticon = "file"
    )
  ),
  use.names = TRUE,
  fill = TRUE
)


set_nested <- function(x, path, value) {
  if (length(path) == 1) {
    if (!is.null(x[[path]])) {
      return(x)
    }
    x[[path]] <- value
    return(x)
  }
  x[[path[1]]] <- set_nested(x[[path[1]]], path[-1], value)
  x
}


tree_create <- function(config) {
  tree <- list()

  for (i in seq_len(nrow(config))) {
    row <- config[i]

    paths <- strsplit(row$path, split = "/")[[1]]
    for(k in seq_len(length(paths) - 1)) {
      # set branch
      tree <- set_nested(
        x = tree,
        path = paths[1:k],
        value = structure(
          list(),
          sticon = "folder"
        )
      )
    }

    # set leaf
    tree <- set_nested(
      x = tree,
      path = paths,
      value = structure(
        list(row$content),
        sticon = row$sticon
      )
    )
  }

  return(tree)
}

tree <- tree_create(config)



tree_visualize <- function(tree) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$script(shiny::HTML(
          "Shiny.addCustomMessageHandler('treeAction', function(message) {
            var tree = $('#tree').jstree(true);
            if (!tree) return;
            if (message === 'expand') tree.open_all();
            if (message === 'collapse') tree.close_all();
          });"
        ))
      ),
      shiny::actionButton(inputId = "expand", label = "expand"),
      shiny::actionButton(inputId = "collapse", label = "collapse"),
      shinyTree::shinyTree(outputId = "tree", stripes = TRUE)
    ),
    server = function(input, output, session) {
      output$tree <- shinyTree::renderTree({
        tree
      })
      shiny::observeEvent(input$expand, {
        session$sendCustomMessage("treeAction", "expand")
      })

      shiny::observeEvent(input$collapse, {
        session$sendCustomMessage("treeAction", "collapse")
      })
    }
  )
}

#tree_visualize(tree)









ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$script(shiny::HTML(
      "Shiny.addCustomMessageHandler('treeAction', function(message) {
          var tree = $('#tree').jstree(true);
          if (!tree) return;
          if (message === 'expand') tree.open_all();
          if (message === 'collapse') tree.close_all();
        });"
    )),
    shiny::tags$style(shiny::HTML("
      #sidebar {
        width: 300px;
        float: left;
        border-right: 1px solid #ddd;
        padding: 10px;
      }
      #main {
        margin-left: 320px;
        padding: 10px;
      }
    "))
  ),

  shiny::div(
    id = "sidebar",
    shiny::actionButton(inputId = "expand", label = "expand"),
    shiny::actionButton(inputId = "collapse", label = "collapse"),
    shiny::h4("Directory"),
    shinyTree::shinyTree("tree")
  ),

  shiny::div(
    id = "main",
    shiny::h3(shiny::textOutput("title")),
    shiny::uiOutput("content")
  )
)

server <- function(input, output, session) {
  shiny::observeEvent(input$expand, {
    session$sendCustomMessage("treeAction", "expand")
  })

  shiny::observeEvent(input$collapse, {
    session$sendCustomMessage("treeAction", "collapse")
  })

  output$tree <- shinyTree::renderTree({
    tree
  })

  # React to tree selection
  shiny::observeEvent(
    eventExpr = input$tree, {
    sel <- shinyTree::get_selected(input$tree, format = "names")

    if (length(sel) == 0) return()

    path <- paste(sel, collapse = " / ")

    output$title <- shiny::renderText(path)

    output$content <- shiny::renderUI({
      shiny::tagList(
        shiny::p("You selected:"),
        shiny::tags$pre(path),
        shiny::p("This is where your page content would go.")
      )
    })
  })
}

shiny::shinyApp(ui, server)

