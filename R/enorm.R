##########################################
#' Fit the extended nominal response model on MST data
#'
#' Fits an Extended NOminal Response Model (ENORM) using conditional maximum likelihood (CML)
#' 
#' 
#' @param db an dextermst db handle
#' @param predicate logical predicate to select data to include in the analysis, see details
#' @param fixed_parameters data.frame with columns `item_id`, `item_score` and `beta`
#' 
#' @return
#' object of type 'mst_enorm'. Can be cast to a data.frame of item parameters 
#' using function `coef` or used in dexter's \code{\link[dexter]{ability}} functions
#' 
#' @details 
#'  You can use the predicate to include or omit responses from the analysis, e.g.
#'  `p = fit_enorm_mst(db, item_id != 'some_item' & student_birthdate > '2005-01-01')`
#'  
#' DexterMST will automatically correct the routing rules for the purpose of the current analysis. 
#' There are some caveats though. Predicates that lead to many different designs, e.g. a predicate like
#' \code{response != 'NA'} (which is perfectly valid but can potentially create 
#' almost as many tests as there are students) might take very long to compute. 
#' 
#' Predicates that remove complete modules from a test, e.g. \code{module_nbr !=2} or \code{module_id != 'RU4'} 
#' will cause an error and should be avoided. 
#' 
#' 
#' @references 
#' Zwitser, R. J. and Maris, G (2015); Conditional statistical inference with multistage testing designs. 
#' Psychometrika. Vol. 80, no. 1, 65-84.
#' 
fit_enorm_mst = function(db, predicate = NULL, fixed_parameters = NULL)
{
  qtpredicate = eval(substitute(quote(predicate)))
  env = caller_env() 
  fit_enorm_mst_(db, qtpredicate, fixed_parameters, env)
}

fit_enorm_mst_ = function(db, qtpredicate, fixed_parameters=NULL, env)
{
  mst_inputs = get_mst_data(db, qtpredicate, env)
  
  if(length(mst_inputs$bkList) == 0 )
    stop('no responses to analyze')
  
  # test if connected
  im = as.matrix(table(mst_inputs$booklet_design$item_id, 
                       paste(mst_inputs$booklet_design$test_id, mst_inputs$booklet_design$booklet_id, sep='.')))
  wm = crossprod(im, im)
  diag(wm) = 0
  
  visited = rep(FALSE, ncol(wm))
  
  dfs = function(start)
  {
    if(!visited[start])
    {
      visited[start] <<- TRUE
      vapply((1:nrow(wm))[wm[,start]>0], dfs, 0L)
    }
    0L
  }
  dfs(1)
  if(!all(visited)) stop('your design is not connected')
  
  if (!is.null(fixed_parameters))
  {
    if(length(setdiff(c('item_id','item_score','beta'), colnames(fixed_parameters))) > 0)
      stop('fixed_parameters needs to have the following columns: (item_id, item_score, beta)')
    
    fixed_not_found = fixed_parameters %>%
      distinct(.data$item_id) %>%
      anti_join(mst_inputs$ssI, by='item_id') %>%
      pull(.data$item_id)
    
    if(length(fixed_not_found) > 0)
    {
      message('some of your fixed parameters have no match in the data, they will be ignored')
      message(paste(unique(fixed_not_found), collapse=' '))
    }
    fixed_parameters = fixed_parameters %>%
      arrange(.data$item_id, .data$item_score) %>%
      mutate(rn = row_number())
    
    ffl = fixed_parameters %>%
      group_by(.data$item_id) %>%
      summarise(first = min(.data$rn), last=max(.data$rn))
    
    dx = dexter.toDexter(fixed_parameters$beta, fixed_parameters$item_score, ffl$first, ffl$last, 
                    re_normalize=FALSE)
    dx_b = dx$est$b[-dx$inputs$ssI$first]
    
    fixed_parameters = fixed_parameters %>% 
      add_column(b = dx_b)
    
    fixed_parameters = mst_inputs$ssIS %>%
      select(.data$item_id, .data$item_score) %>%
      left_join(fixed_parameters, by=c('item_id','item_score')) %>%
      arrange(.data$item_id, .data$item_score) %>%
      pull(.data$b)
    
    if(!anyNA(fixed_parameters))
      stop('Nothing to calibrate, all item parameters have been fixed')
    if(all(is.na(fixed_parameters)))
      stop('none of your fixed parameters belong to items in the data')
  }
  
  if(any(mst_inputs$ssIS$sufI == 0))
  {
    cat('score categories without responses:\n')
    mst_inputs$ssIS %>% filter(.data$sufI == 0) %>% select(.data$item_id, .data$item_score) %>%
      as.data.frame() %>% print(row.names=FALSE)
    stop('Score categories without observations, see output')
  }
  
  res = Calibrate_MST(booklets = mst_inputs$bkList, 
                      a = pull(mst_inputs$ssIS, 'item_score'), 
                      sufI = pull(mst_inputs$ssIS, 'sufI'),
                      fixed_b = fixed_parameters)
  
  
  abl_tables = new.env(parent = emptyenv())
  abl_tables$mle = NULL
  

  
  out = list(mst_est = bind_cols(select(mst_inputs$ssIS, .data$item_id, .data$item_score), 
                                 res[names(res) != 'acov.beta']), 
             mst_inputs = mst_inputs,
             abl_tables = abl_tables,
             inputs = list(method = 'CML',
                           ssI = mst_inputs$ssI %>% mutate(first = as.integer(first + rank(first) - 1L), last = as.integer(last + rank(last))),
                           ssIS = bind_rows(mst_inputs$ssIS, 
                                            tibble(item_id=pull(mst_inputs$ssI, 'item_id'), item_score = 0L, 
                                                   sufI = as.integer(NA))) %>% 
                                    arrange(.data$item_id, .data$item_score),
                           bkList = mst_inputs$bkList,
                           has_fixed_parms = !is.null(fixed_parameters)))
  
  out$inputs$plt = out$mst_inputs$plt
  out$mst_inputs$plt = NULL
  # Prepare to label vectors/matrices
  it_sc_lab = paste0(out$inputs$ssIS$item_id[-out$inputs$ssI$first], 
                     "_", out$inputs$ssIS$item_score[-out$inputs$ssI$first])
  
  # for compatibility with dexter
  out$est = dexter.toDexter(out$mst_est$beta, out$mst_est$item_score, 
                            out$mst_inputs$ssI$first, out$mst_inputs$ssI$last)$est
  out$est$acov.beta = res$acov.beta
  out$est$beta.cml = out$mst_est$beta # for backward compatibility
  
  class(out) = append(c('mst_enorm', 'prms') ,class(out))
  out
}


print.mst_enorm = function(x, ...)
{
  msg = paste0('Parameters for the Extended Nominal Response model based on Multi Stage calibration\n\n',
               nrow(x$inputs$ssI), ' items\n\n',
               'Use coef() to retrieve the item parameters\n')
  cat(msg)
  invisible(msg)
}

#' extract enorm mst item parameters
#' 
#' @param object an 'mst_enorm' parameters object, generated by the function \code{\link{fit_enorm_mst}}
#' @param ... other parameters are currently ignored
#' 
#' @return 
#' a data.frame with columns: item_id, item_score, beta, SE_b
#' 
coef.mst_enorm = function(object, ...)
{
  # beetje lastig, namen gelijktrekken aan dexter 
  object$mst_est %>% 
    select(-.data$E, -.data$O, -.data$b, -.data$eta) %>% 
    rename(SE_beta = 'se.cml') %>%
    mutate_if(is.matrix, as.vector) %>%
    as.data.frame()
}


