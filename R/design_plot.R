



#' Plot the routing design of mst tests
#' 
#' 
#' @param db dexterMST project database connection
#' @param predicate logical predicate to select data (tests, booklets,responses) to include in the design plot
#' @param ... further arguments to \code{\link[igraph]{plot.igraph}}, see: \code{\link[igraph]{igraph.plotting}}
#' 
#' @details
#' You can use this function to plot routing designs for tests before or after
#' they are administered. There are some slight differences.
#' 
#' If you have entered response data already, the thickness of the line will indicate the numbers
#' of respondents that took the respective paths through the test. Paths not taken will not be drawn. You can use the
#' predicate (see examples) to include or exclude items, tests and respondents.
#' 
#' If you have not entered response data, all lines will have equal thickness. Variables you can use in the predicate
#' are limited to test_id and booklet_id in this case.   
#' 
#' 
#' 
#' 
#' @examples
#' \dontrun{
#' # plot test designs for all tests in the project
#' design_plot(db)
#' 
#' # plot design for a test with id 'math'
#' design_plot(db, test_id == 'math')
#' 
#' # plot design for test math with item 'circumference' turned off
#' # (this plot will only work if you have response data)
#' design_plot(db, test_id == 'math' & item_id != 'circumference')
#' 
#' }
#' 
design_plot = function(db, predicate = NULL, ...)
{
  user.args = list(...)
  qtpredicate = eval(substitute(quote(predicate)))
  env = caller_env() 
  
  if(inherits(db,'mst_enorm'))
  {
    # for now hidden functionality
    if(!is.null(qtpredicate))
      stop('predicates only with database')
    mst_inputs = db$mst_inputs
  } else
  {
    mst_inputs = get_mst_data(db, qtpredicate, env)
  }
    
  if(length(mst_inputs$bkList) == 0)
  {
    #have_data = (nrow(dbGetQuery(db,'SELECT 1 FROM Responses LIMIT 1;')) > 0)
    
    design = dbGetQuery(db, 
                        'SELECT * FROM booklet_design 
                        INNER JOIN Tests USING(test_id)
                        INNER JOIN Booklets USING(test_id, booklet_id);')
    
    if(!is.null(qtpredicate))
    {
      vrs = all.vars(qtpredicate)
      if(length(setdiff(vrs, c(dbListFields(db, 'Tests'), dbListFields(db, 'Booklets')))) > 0)
      {
        message('No Responses selected')
        return()
      }
      design = design[eval_tidy(qtpredicate, data = design, env=env), ]
    }
    
    am = design %>%
      arrange(.data$test_id, .data$booklet_id, .data$module_nbr) %>%
      group_by(.data$test_id, .data$booklet_id) %>%
      mutate(mfrom = .data$module_id, mto = lead(.data$module_id),
             label = paste(.data$module_exit_score_min, .data$module_exit_score_max, sep=':'), n=0L) %>%
      slice(1:(n()-1)) %>%
      ungroup()
  } else
  {
    am = lapply(mst_inputs$bkList, 
                function(b)
                {
                  if(length(b$modules) == 1)
                  {
                    tibble(mfrom=b$modules[1],mto=b$modules[1],label='',n=1)
                  } else
                  {
                    tibble(mfrom = b$modules[1:length(b$modules)-1], 
                           mto = b$modules[2:length(b$modules)],
                           label = paste(b$min_scores, b$max_scores, sep=':')[1:length(b$modules)-1],
                           n = sum(b$scoretab))
                  }
                }) %>%
      bind_rows(.id = 'booklet') %>%
      separate(.data$booklet, into = c('test_id','booklet_id'), sep = '\\.')
  }
  
  
  op = par(mar=c(0,0,0,0))
  on.exit({par(op)}, add=TRUE)
  
  
  am_summed = am %>%
    group_by(.data$test_id, .data$mfrom, .data$mto, .data$label) %>%
    summarise(width = sum(.data$n)) %>%
    ungroup() %>%
    mutate(width = .data$width/max(.data$width,1) * 10) %>%
    mutate(width = pmax(1L,.data$width))
  
  lapply(split(am_summed, am_summed$test_id), function(amsb)
  {
    g = graph.data.frame(select(amsb,2:5)) %>% simplify(remove.multiple=FALSE, remove.loops=TRUE)
    
    ## Decompose the graph, individual layouts
    comp = decompose.graph(g)
    roots = sapply(lapply(comp, topological.sort), head, n=1)
    coords = mapply(FUN = layout.reingold.tilford, comp,
                    root = roots, SIMPLIFY=FALSE)
    
    ## Put the nodes side by side, roots on the top
    width = sapply(coords, function(x) { r <- range(x[, 1]); r[2] - r[1] })
    gap = 0.5
    shift = c(0, cumsum(width[-length(width)] + gap))
    ncoords = mapply(FUN=function(mat, shift) {
      mat[,1] = mat[,1] - min(mat[,1]) + shift
      mat[,2] = mat[,2] - max(mat[,2])
      mat
    }, coords, shift, SIMPLIFY=FALSE)
    
    ## Put together the coordinates for the original graph,
    ## based on the names of the vertices
    lay = matrix(0, ncol=2, nrow = vcount(g))
    for (i in seq_along(comp)) {
      lay[match(V(comp[[i]])$name, V(g)$name),] <- ncoords[[i]]
    }
    
    ## Plot everything
    
    if(!is.null(mst_inputs$module_design_history))
    {
      mdl = mst_inputs$module_design_history %>%
        group_by(.data$miid, .data$module_id) %>%
        summarise(vlabel = paste0(.data$item_id[.data$rsp_incl == 0], collapse = ',')) %>%
        ungroup() %>%
        mutate(vlabel = paste0(.data$module_id, ifelse(.data$vlabel=='','','\n-'), .data$vlabel), miid = as.character(.data$miid))
      
      vlab = as_tibble(vertex_attr(g)) %>%
        mutate(indx = row_number()) %>%
        inner_join(mdl, by = c(name='miid')) %>%
        arrange(.data$indx) %>%
        pull(.data$vlabel)
      
      do.call(plot, modifyList(list(x=g, layout=lay, vertex.shape='rectangle',
                                    vertex.size=45,vertex.size2=20,vertex.label=vlab),
                               user.args))
    } else
    {
      do.call(plot, modifyList(list(x=g, layout=lay, vertex.shape='rectangle',
                                    vertex.size=45,vertex.size2=20),
                               user.args))
    }
  })

  invisible(NULL)
}

