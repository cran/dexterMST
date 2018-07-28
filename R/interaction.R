

#' Fit the interaction model on a single multi-stage booklet
#' 
#' @param db a db handle
#' @param test_id id of the test as defined in \code{\link{create_mst_test}}
#' @param booklet_id id of the booklet as defined in \code{\link{create_mst_test}}
#' 
fit_inter_mst = function(db, test_id, booklet_id)
{
  prd = list(test_id = test_id, booklet_id = booklet_id)
  design = dbGetQuery(db, 'SELECT * FROM Booklet_design WHERE test_id=:test_id AND booklet_id=:booklet_id ORDER BY module_nbr;', prd)
  
  mdl_item_scorecat = dbGetQuery(db, 
                                 'SELECT DISTINCT module_nbr, item_id, item_score
                                 FROM Booklet_design
                                 INNER JOIN Module_design USING(test_id, module_id)
                                 INNER JOIN Scoring_rules USING(item_id)
                                 WHERE test_id=:test_id AND booklet_id=:booklet_id AND item_score > 0
                                 ORDER BY module_nbr, item_id, item_score;', 
                                 prd)
  
  resp = dbGetQuery(db,
                    'SELECT person_id, item_id, item_score
                    FROM Responses 
                    INNER JOIN Scoring_rules USING(item_id, response)
                    WHERE test_id=:test_id AND booklet_id=:booklet_id;',
                    prd)

  routing = dbGetQuery(db,'SELECT routing FROM Tests WHERE test_id=:test_id;',list(test_id=test_id))$routing
  
  
  resp = resp %>%
    group_by(.data$person_id) %>%
    mutate(booklet_score = sum(.data$item_score)) %>%
    ungroup() 
  
  ssIS = resp %>%
    filter(.data$item_score > 0) %>%
    group_by(.data$item_id, .data$item_score) %>%
    summarise(sufI = n(), sufC = sum(.data$item_score * .data$booklet_score)) %>%
    ungroup() %>%
    right_join(mdl_item_scorecat, by=c('item_id', 'item_score')) %>%
    mutate(sufI = coalesce(.data$sufI, 0L), sufC = coalesce(.data$sufC, 0L)) %>%
    arrange(.data$module_nbr, .data$item_id, .data$item_score) 
  
  ssI = ssIS %>%
    mutate(indx = row_number()) %>%
    group_by(.data$module_nbr, .data$item_id) %>%
    summarise(first = min(.data$indx), last = max(.data$indx), nCat = n(), sufC=sum(.data$sufC)) %>%
    ungroup() %>%
    arrange(.data$module_nbr, .data$item_id)
  
  bkl_max = mdl_item_scorecat %>%
    group_by(.data$item_id) %>%
    summarise(mx = max(.data$item_score)) %>%
    pull(.data$mx) %>%
    sum()
  
  scoretab = resp %>%
    distinct(.data$person_id, .keep_all = TRUE) %>%
    group_by(.data$booklet_score) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    right_join(tibble(booklet_score = 0:bkl_max), by = 'booklet_score') %>%
    mutate(n = coalesce(.data$n, 0L)) %>%
    arrange(.data$booklet_score)
  
  
  res = Estim_MST(a = pull(ssIS, .data$item_score), 
                  first = split(pull(ssI, .data$first), ssI$module_nbr), 
                  last = split(pull(ssI, .data$last), ssI$module_nbr), 
                  min_scores = pull(design, .data$module_exit_score_min), 
                  max_scores = pull(design, .data$module_exit_score_max), 
                  sufI = pull(ssIS, .data$sufI), 
                  sufC = pull(ssI, .data$sufC),
                  scoretab = pull(scoretab, .data$n), 
                  routing = routing)
  
  # regressions for plots
  observed = resp %>%
    group_by(.data$item_id, .data$booklet_score) %>%
    summarise(mean_item_score = mean(.data$item_score)) %>%
    ungroup()
  
  C = rep(1:nrow(ssI), pull(ssI, .data$nCat))
  if (routing=="all")
  {
    ctrRM = ittotmat_MS_all(res$bRM, pull(ssIS, .data$item_score),  
                            first = split(pull(ssI, .data$first), ssI$module_nbr), 
                            last = split(pull(ssI, .data$last), ssI$module_nbr),  
                            min_scores = pull(design, .data$module_exit_score_min), 
                            max_scores = pull(design, .data$module_exit_score_max))
    
    ctrIM = ittotmatIM_MS_all(res$bIM, res$cIM[C], pull(ssIS, .data$item_score), 
                              first = split(pull(ssI, .data$first), ssI$module_nbr), 
                              last = split(pull(ssI, .data$last), ssI$module_nbr),  
                              min_scores = pull(design, .data$module_exit_score_min), 
                              max_scores = pull(design, .data$module_exit_score_max))
  }
  
  if (routing=="last")
  {
    ctrRM = ittotmat_MS_last(res$bRM, pull(ssIS, .data$item_score),  
                             first = split(pull(ssI, .data$first), ssI$module_nbr), 
                             last = split(pull(ssI, .data$last), ssI$module_nbr),  
                             min_scores = pull(design, .data$module_exit_score_min), 
                             max_scores = pull(design, .data$module_exit_score_max))
    
    ctrIM = ittotmatIM_MS_last(res$bIM, res$cIM[C], pull(ssIS, .data$item_score), 
                               first = split(pull(ssI, .data$first), ssI$module_nbr), 
                               last = split(pull(ssI, .data$last), ssI$module_nbr),  
                               min_scores = pull(design, .data$module_exit_score_min), 
                               max_scores = pull(design, .data$module_exit_score_max))
  }
  
  #mm = matrix(0, nrow(ssIS), nrow(ssIS))
  #diag(mm) = pull(ssIS, .data$item_score)
  mm = sweep(model.matrix(~0+ssIS$item_id), 1, ssIS$item_score, '*')

  itrRM = as.data.frame(crossprod(mm, ctrRM))
  itrIM = as.data.frame(crossprod(mm, ctrIM))
  row.names(itrRM) = row.names(itrIM) = ssI$item_id
  
  regs = list(ctrRM = ctrRM, ctrIM = ctrIM, itrRM = itrRM, itrIM = itrIM, observed = observed)
  
  out = list(inputs = list(ssI = ssI, ssIS = ssIS, scoretab = scoretab, design = design, routing = routing,
                           test_id=test_id, booklet_id=booklet_id), 
             pars = res, regs = regs)
  class(out) = append('im_mst', class('out'))
  out
}

#' plots for the interaction model
#' 
#' @param x output of \code{\link{fit_inter_mst}}
#' @param item_id id of the item to plot
#' @param show.observed plot the observed mean item scores for each test score
#' @param curtains percentage of most extreme values to cover with curtains, 0 to omit curtains
#' @param zoom if TRUE, limits the plot area to the test score range allowed by the routing rules
#' @param ... further arguments to plot
#' 
plot.im_mst = function(x, item_id, show.observed = TRUE, curtains = 10, zoom = FALSE, ...){
  
  user.args = list(...)
  zI = x$regs$itrIM
  zR = x$regs$itrRM
  if(zoom)
  {
    max_booklet = max(x$inputs$design$module_exit_score_max)
    min_booklet = max(x$inputs$design$module_exit_score_min)
  } else
  {  
    max_booklet = nrow(x$inputs$scoretab) - 1
    min_booklet = 0
  }
  if(! item_id %in% x$inputs$ssIS$item_id)
    stop(paste0('item "',item_id,'" is not part of this analysis'))
  
  itm = item_id
  
  max_item = max(filter(x$inputs$ssIS, .data$item_id == itm) %>% pull(.data$item_score))
  
  mdl_nbr = filter(x$inputs$ssI, .data$item_id == itm) %>% pull(.data$module_nbr)
  mdl_id = filter(x$inputs$design, .data$module_nbr == mdl_nbr) %>% pull(.data$module_id)
  
  
  
  do.call(plot, modifyList(list(x = c(min_booklet, max_booklet),y = c(0, max_item), type="n",
                                main = paste0(item_id, ', module ', mdl_nbr, " (", mdl_id, ")") , 
                                xlab = 'test score', ylab = 'item score', bty='l'),
                           user.args))
  
  
  if(curtains > 0)
  {
    qnt = x$inputs$scoretab %>%
      filter((cumsum(.data$n) <= sum(.data$n) * (100 - 0.5 * curtains)/100) & (cumsum(.data$n) >= sum(.data$n) * 0.5 * curtains/100))  %>%
      summarise(mn = min(.data$booklet_score), mx = max(.data$booklet_score))
    
    usr = graphics::par('usr')
    graphics::rect(usr[1], usr[3], pull(qnt, .data$mn), usr[2], col="#EEEEEE", border=NA,xpd=FALSE,lwd=0)
    graphics::rect(pull(qnt, .data$mx), usr[3], usr[2], usr[4], col="#EEEEEE", border=NA,xpd=FALSE,lwd=0)
    abline(h=usr[3])
    abline(v=usr[1])
  }
  
  if(show.observed) 
  {
    obs = filter(x$regs$observed, .data$item_id == itm)
    points(pull(obs, .data$booklet_score), pull(obs, .data$mean_item_score),col="coral",pch=20)
  }
  
  lines(min_booklet:max_booklet, zI[row.names(zI)==item_id,(min_booklet:max_booklet)+1 ], col="gray80", lwd=3)
  lines(min_booklet:max_booklet, zR[row.names(zR)==item_id,(min_booklet:max_booklet)+1])
  invisible(NULL)
}


print.im_mst = function(x, ...)
{
  routing = paste0(x$inputs$design$module_id, 
                   paste0('[',x$inputs$design$module_exit_score_min,':',x$inputs$designmodule_exit_score_max,']'),
                  collapse=' --+ ')
  
  cat(paste0('Interaction model parameters for booklet "',x$inputs$booklet_id,'" in test "',x$inputs$test_id,
             '"\n\ndesign: ',routing,
             '\n\nn items: ',nrow(x$inputs$ssI),'\nn persons: ',sum(x$inputs$scoretab$n),'\n'))
  invisible(x)
}


coef.im_mst = function(object, ...)
{
  object$inputs$ssIS %>%
    select(.data$item_id, .data$item_score) %>%
    add_column(bRM = object$pars$bRM, cRM = object$pars$cRM, bIM = object$pars$bIM, cIM = object$pars$cIM,
               se.c = object$pars$se.c, fit.IM = object$pars$fit.stats)
}


