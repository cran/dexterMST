



qtpredicate2sql = function(qtpredicate, db, env)
{
  vars = unique(c(dbListFields(db,'Items'), dbListFields(db,'Booklets'), dbListFields(db,'Persons'), 
                  dbListFields(db,'Booklet_design'),dbListFields(db,'Responses'),dbListFields(db,'Administrations'),
                  dbListFields(db,'Tests'),dbListFields(db,'Modules'),dbListFields(db,'Module_design')))
  
  if(is.list(qtpredicate))
  {
    sql = lapply(qtpredicate, function(qtp){
      translate_sql_(list(partial_eval(qtp,vars=vars,env=env)),con=db)
    })
  } else
  {
    sql = translate_sql_(list(partial_eval(qtpredicate,vars=vars,env=env)),con=db)
  }
  
  # translate_sql bug occurs for expressions like: booklet_id %in% as.character(1:4) 
  # <SQL> `booklet_id` IN CAST((1, 2, 3, 4) AS TEXT)
  # unfortunately regular expression functionality in R is a bit awkward 
  # so this is extremely kludgy to solve
  sql = lapply(sql, function(str)
  {
    split = strsplit(str,"CAST((", fixed=TRUE)[[1]]
    if(length(split)==1) return(str)
    split[1] = paste0(split[1],'(')
    split[2:length(split)] = vapply(split[2:length(split)], function(s)
    {
      mvec = regexpr('^.+(?=\\) AS )', s, perl=TRUE)
      vec = regmatches(s, mvec)
      remainder = substring(s,attr(mvec,'match.length')+6)
      mdtype = regexpr('^\\w+', remainder, perl=TRUE)
      dtype = regmatches(remainder, mdtype)
      remainder = substring(s, attr(mvec,'match.length') + 6 + attr(mdtype,'match.length')) 
      
      vec = trimws(vec)
      if(substr(vec,1,2) == "'")
      {
        #pff, strings. embedded quotes are already doubled
        vec = substr(vec, 2, nchar(vec)-1)			
        vec = paste0("CAST('", 
                     strsplit(vec,"(?<![^']')', ?'", perl=TRUE)[[1]],
                     "' AS ", dtype, ')', 
                     collapse=',') 
      } else
      {
        vec = paste0('CAST(', strsplit(vec, ',')[[1]], ' AS ', dtype, ')', collapse=',') 
      }
      paste0(vec, remainder)
    },"")
    paste0(split, collapse=' ')
  }) %>% unlist()
  
  if(length(sql) > 1) sql = paste0('(',sql,')',collapse=' AND ')
  
  return(sql)
}

#' Extract response data from a dexterMST database
#' 
#'
#' @param db a dexterMST project database connection
#' @param predicate an expression to select data on
#' @param columns the columns you wish to select, can include any column in the project
#' @return a data.frame of responses
#' 
get_responses_mst = function(db, predicate = NULL, 
                             columns=c('person_id', 'test_id', 'booklet_id', 'item_id', 'item_score'))
{
  qtpredicate = eval(substitute(quote(predicate)))
  env = caller_env() 
  
  get_rsp_data(db, qtpredicate, env=env, columns = columns)
}


get_rsp_data = function(db, qtpredicate=NULL, env=NULL,
                        columns=c('person_id', 'test_id', 'booklet_id', 'item_id', 'item_score'),
                        filter_col = NULL)
{
  cte = " Responses INNER JOIN Scoring_rules USING(item_id, response)"
  
  if(!is.null(qtpredicate)){
    used_columns = union(columns, tolower(all.vars(qtpredicate))) 
    suppressWarnings({ sqlpredicate = qtpredicate2sql(qtpredicate, db, env)})
    
    # very weird, but this gives wrong results sometimes
    #where = paste('WHERE', sqlpredicate)
    # whereas this gives correct results seemingly
    where = paste('WHERE CASE WHEN (', sqlpredicate, ') THEN 1 ELSE 0 END = 1')
    #where3 = paste('WHERE (', sqlpredicate, ') is NULL')

  } else
  {
    used_columns = columns
    where = ''
  }
    
  if(length(intersect(dbListFields(db,'Persons'),used_columns))>0) 
    cte = c(cte, 'INNER JOIN Persons USING(person_id)')
  if(length(intersect(dbListFields(db,'Items'),used_columns))>0) 
      cte = c(cte, 'INNER JOIN Items USING(item_id)')
  if(length(intersect(dbListFields(db,'Administrations'),used_columns))>0) 
      cte = c(cte, 'INNER JOIN Administrations USING(person_id, test_id, booklet_id)')
  if(length(intersect(dbListFields(db,'Tests'),used_columns))>0) 
      cte = c(cte, 'INNER JOIN Tests USING(test_id)')
  if(length(intersect(dbListFields(db,'Booklets'),used_columns))>0) 
      cte = c(cte, 'INNER JOIN Booklets USING(test_id, booklet_id)')
  if(length(intersect(dbListFields(db,'Modules'),used_columns))>0) 
      cte = c(cte, 'INNER JOIN Modules USING(test_id, module_id)')
  if(length(intersect(dbListFields(db,'Module_design'),used_columns))>0) 
      cte = c(cte, 'INNER JOIN Module_design USING(test_id, module_id, item_id)')
  if(length(intersect(dbListFields(db,'Booklet_design'),used_columns))>0) 
      cte = c(cte, 'INNER JOIN Booklet_design USING(test_id, booklet_id, module_id)')
    

  respData = NULL
  tryCatch(
    {
      if(is.null(filter_col))
      {
        respData = dbGetQuery(db, 
                            paste("SELECT", 
                                  paste0(columns, collapse=','),
                                  "FROM",
                                  paste0(cte,collapse=" "),
                                  where,';'))
      } else
      {
        # filtercol necessitates a predicate
        respData = dbGetQuery(db, 
                            paste("SELECT", 
                                  paste0(columns, collapse=','),',',
                                  'CASE WHEN (',sqlpredicate,') THEN 1 ELSE 0 END AS ', filter_col,
                                  " FROM ",
                                  paste0(cte,collapse=" ")))
      }
      
    },
    error = function(e)
    {
      #print('error')
      if(grepl('(syntax error)|(no such function)', e$message, ignore.case=TRUE))
      {
        # could be that dbplyr cannot translate the r syntax since it contains non sql functions
        # try to read all data (no where clause) and run the select on that
        
        respData <<- dbGetQuery(db,
                                paste("SELECT",
                                      paste0(intersect(union(columns, all.vars(qtpredicate)),
                                                       get_mst_variables(db)$name), collapse=','),
                                      " FROM ",
                                      paste0(cte,collapse=" ")))
        if(is.null(filter_col))
        {
          respData <<- respData[eval_tidy(qtpredicate, data = respData, env=env), columns]
        } else 
        {
          respData <<- respData %>% add_column(rsp_incl = as.integer(eval_tidy(qtpredicate, data = respData, env=env)))
        }
      } else
      {
        stop(e)
      }
    },
    finally = NULL)
  return(respData)
}



mst_safe = function(db, qtpredicate){
  blacklist = 
    setdiff(
      Reduce(union, list(
        dbListFields(db, 'Scoring_rules'),
        dbListFields(db, 'Booklet_design'),
        dbListFields(db, 'Module_design'),
        dbListFields(db, 'Items'),
        dbListFields(db, 'Scoring_rules'))),
      c('test_id', 'booklet_id')) 
  
  if(is.list(qtpredicate))
  {
    vrs = unlist(lapply(qtpredicate,all.vars))
  } else
  {
    vrs = all.vars(qtpredicate)
  }
  
  return(length(intersect(tolower(vrs),tolower(blacklist))) == 0)
}


get_mst_variables = function (db) 
{
  lapply(dbListTables(db), 
         function(tbl) 
         {
           res = DBI::dbSendQuery(db, paste("SELECT * FROM", tbl, 
                                            "WHERE 0=1;"))
           r = DBI::dbColumnInfo(res)
           DBI::dbClearResult(res)
           return(r)
         }) %>% 
    bind_rows() %>% 
    distinct() %>% 
    arrange(.data$name)
}

get_mst_data = function(db, qtpredicate=NULL, env=NULL)
{
  if(is.null(env)) env = caller_env() 
  
  if(is.null(qtpredicate) || mst_safe(db, qtpredicate))
  {
    return(safe_mst_data(db, qtpredicate, env))
  } else
  {
    return(unsafe_mst_data(db, qtpredicate, env))
  }
}



safe_mst_data = function(db, qtpredicate, env){
  # avoids all time consuming computations for mutilating routing rules, etc.
  
  columns = c('person_id', 'test_id', 'booklet_id', 'item_id', 'item_score')
  
  respData = get_rsp_data(db, qtpredicate=qtpredicate, env=env,
                          columns=columns,
                          filter_col = NULL)
  
  
  plt = respData %>%
    group_by(.data$person_id, .data$test_id, .data$booklet_id) %>%
    summarise(booklet_score = sum(.data$item_score)) %>%
    ungroup() 
  
  scoretab = plt %>%
    group_by(.data$test_id, .data$booklet_id, .data$booklet_score) %>%
    summarise(n = n()) %>%
    ungroup()
  
  plt = plt %>% 
    inner_join(respData, by = c('person_id','test_id', 'booklet_id')) %>%
    mutate(booklet_id = paste(.data$test_id, .data$booklet_id, sep='.')) %>%
    rename(sumScore = 'booklet_score') %>%
    group_by(.data$booklet_id, .data$item_id, .data$sumScore) %>% 
    summarise(meanScore=mean(.data$item_score), N=n()) %>% 
    ungroup()
  
  ssIS = respData %>%
    filter(.data$item_score > 0) %>%
    group_by(.data$item_id, .data$item_score) %>%
    summarise(sufI = n()) %>% 
    ungroup()
  
  item_scores = dbGetQuery(db, 'SELECT DISTINCT item_id, item_score FROM scoring_rules WHERE item_score > 0;') %>%
    semi_join(ssIS, by='item_id')
  
  ssIS = ssIS %>% 
    right_join(item_scores, by=c('item_id','item_score')) %>%
    mutate(sufI = coalesce(.data$sufI, 0L)) %>%
    arrange(.data$item_id, .data$item_score)
  
  ssI = ssIS %>%
    mutate(rn = row_number()) %>%
    group_by(.data$item_id) %>%
    summarise(first = min(.data$rn), last = max(.data$rn), item_max_score = max(.data$item_score)) %>%
    ungroup() %>%
    arrange(.data$item_id)
  
  
  routing = dbGetQuery(db, 'SELECT * FROM Tests INNER JOIN booklet_design USING(test_id);') %>%
    semi_join(scoretab, by=c('test_id','booklet_id')) %>%
    arrange(.data$test_id, .data$booklet_id, .data$module_nbr)
  
  module_design = dbGetQuery(db, 'SELECT test_id, module_id, item_id FROM Module_design;') 
  
  booklet_items = routing %>%
    select(.data$test_id, .data$booklet_id, .data$module_id, .data$module_nbr) %>%
    inner_join(module_design, by=c('test_id','module_id')) %>%
    inner_join(ssI, by = 'item_id') %>%
    arrange(.data$test_id, .data$booklet_id, .data$module_nbr, .data$first)
  
  
  booklets = lapply(split(booklet_items, paste(booklet_items$test_id, booklet_items$booklet_id,sep='.')),
                    function(b)
                    {
                      route = filter(routing, 
                                     .data$test_id == b$test_id[1] & .data$booklet_id == b$booklet_id[1])
                      
                      stab = scoretab %>% 
                        filter(.data$booklet_id == b$booklet_id[1] & .data$test_id == b$test_id[1] ) %>%
                        right_join(tibble(booklet_score = 0:sum(b$item_max_score)), by = 'booklet_score') %>%
                        mutate(n = coalesce(n, 0L)) %>%
                        arrange(.data$booklet_score) %>%
                        pull(.data$n)
                      
                      list(booklet_id = paste(b$test_id[1], b$booklet_id[1], sep = '.'),
                           test_id = b$test_id[1],
                           modules = pull(route, .data$module_id),
                           min_scores = pull(route, .data$module_exit_score_min),
                           max_scores = pull(route, .data$module_exit_score_max),
                           routing = pull(route,'routing')[1],
                           items = pull(b, .data$item_id),
                           scoretab = stab,
                           first = split(pull(b, .data$first), b$module_nbr),
                           last = split(pull(b, .data$last), b$module_nbr))
                    })
  
  
  list(bkList = booklets, ssI = ssI, ssIS = ssIS, module_design_history = NULL, 
       plt=plt,
       module_design = semi_join(module_design, routing, by=c('test_id','module_id')),
       booklet_design = select(booklet_items, .data$test_id, .data$booklet_id, .data$item_id))
  
}


unsafe_mst_data = function(db, qtpredicate,  env)
{
  columns=c('person_id', 'test_id', 'booklet_id', 'module_id', 'module_nbr','item_id', 'item_score')
  
  respData = get_rsp_data(db, qtpredicate=qtpredicate, env=env,
                          columns=columns,
                          filter_col = 'rsp_incl')
  
  
  # take out empty person-booklets and take out excluded responses in the last module
  respData = respData %>% 
    group_by(.data$person_id, .data$test_id, .data$booklet_id) %>%
    mutate(bkl_rsp_incl = sum(.data$rsp_incl), mx_module_nbr = max(.data$module_nbr)) %>%
    ungroup() %>%
    filter(.data$bkl_rsp_incl > 0 & (.data$module_nbr < .data$mx_module_nbr | .data$rsp_incl == 1)) %>%
    arrange(.data$person_id, .data$test_id, .data$module_id, .data$item_id) %>%
    select(-.data$mx_module_nbr, -.data$bkl_rsp_incl)

  # define modules
  fitem = factor(respData$item_id)
  respData$iid = fmatch(respData$item_id, fitem)
  
  respData = respData %>% 
    group_by(.data$person_id, .data$test_id, .data$module_id) %>%
    mutate(miid = paste0(.data$iid[as.logical(.data$rsp_incl)], collapse = " ")) %>%
    ungroup()
  
  fmiid = factor(respData$miid)
  respData$miid = fmatch(respData$miid, fmiid)
  
  module_design_history = respData %>% 
    distinct(.data$test_id, .data$module_id, .data$miid, .data$item_id, .data$rsp_incl)
  
  module_design = module_design_history %>%
    filter(.data$rsp_incl == 1) %>%
    distinct(.data$test_id, .data$miid, .data$item_id)
  
  # one row per person per module, make unique bkl id
  # sum excluded is set to 0 for last module
  person_summary = respData %>%
    group_by(.data$person_id, .data$test_id, .data$booklet_id, .data$module_nbr, .data$miid) %>%
    summarise(mdl_rsp_incl = sum(.data$rsp_incl), mdl_rsp = n(),
              mdl_sum_incl = sum(.data$rsp_incl * .data$item_score),
              mdl_sum_excl = sum(.data$item_score) - sum(.data$rsp_incl * .data$item_score)) %>%
    ungroup() %>%
    arrange(.data$person_id, .data$test_id, .data$booklet_id, .data$module_nbr) %>%
    group_by(.data$person_id, .data$test_id, .data$booklet_id) %>%
    mutate(biid = paste(.data$miid, .data$mdl_sum_excl, sep='_', collapse=' ')) %>%
    ungroup()

  
  # scoretab based on biid, biid will now be used in favor of booklet id's
  plt = person_summary %>%
    group_by(.data$person_id, .data$test_id, .data$biid) %>%
    summarise(booklet_score = sum(.data$mdl_sum_incl)) %>%
    ungroup() 
  
  scoretab = plt %>%
    group_by(.data$test_id, .data$biid, .data$booklet_score) %>%
    summarise(n = n()) %>%
    ungroup()
  
  # for plotting in dexter
  plt = respData %>%
    filter(.data$rsp_incl == 1L) %>%
    inner_join(plt, by=c('person_id','test_id')) %>%
    mutate(booklet_id=paste(.data$test_id, .data$biid, sep='.')) %>%
    rename(sumScore = 'booklet_score') %>%
    group_by(.data$booklet_id, .data$item_id, .data$sumScore ) %>%
    summarise(meanScore=mean(.data$item_score), N=n()) %>% 
    ungroup()
  
  
  # items 
  ssIS = respData %>%
    filter(.data$rsp_incl == 1L & .data$item_score > 0) %>%
    group_by(.data$item_id, .data$item_score) %>%
    summarise(sufI = n()) %>% 
    ungroup()

  item_scores = dbGetQuery(db, 'SELECT DISTINCT item_id, item_score FROM scoring_rules WHERE item_score > 0;') %>%
    semi_join(ssIS, by='item_id')
  
  ssIS = ssIS %>% 
    right_join(item_scores, by=c('item_id','item_score')) %>%
    mutate(sufI = coalesce(.data$sufI, 0L)) %>%
    arrange(.data$item_id, .data$item_score)
  
  ssI = ssIS %>%
    mutate(rn = row_number()) %>%
    group_by(.data$item_id) %>%
    summarise(first = min(.data$rn), last = max(.data$rn), max_score = max(.data$item_score)) %>%
    ungroup() %>%
    arrange(.data$item_id)
  
  
  # adjust the routing rules
  booklet_items = person_summary %>%
    distinct(.data$test_id, .data$biid, .data$miid, .data$module_nbr) %>%
    inner_join(module_design, by=c('test_id', 'miid')) %>%
    inner_join(ssI, by = 'item_id') %>%
    arrange(.data$test_id, .data$biid, .data$module_nbr, .data$first)
  
  
  mdl_max_scores = module_design %>%
    inner_join(ssI, by='item_id') %>%
    group_by(.data$test_id, .data$miid) %>%
    summarise(mod_max = sum(.data$max_score)) %>%
    ungroup()
  
  
  lag_mod_max = function(module_exit_score_max, mod_max)
  {
    # de laatste module wordt hier altijd geminimaliseerd op mod_max
    for(i in seq_along(module_exit_score_max))
    {
      if(i==1)
      {
        module_exit_score_max[i] = pmin(module_exit_score_max[i], mod_max[i])
      } else
      {
        module_exit_score_max[i] = pmin(module_exit_score_max[i], module_exit_score_max[i-1] + mod_max[i])
      }
    }
    module_exit_score_max
  }
  
  booklet_design = dbGetQuery(db, 
                              'SELECT *
                                  FROM Tests INNER JOIN Booklet_design USING(test_id)')
  routing = person_summary %>%
    distinct(.data$test_id, .data$booklet_id, .data$biid, .data$module_nbr, .data$miid, .data$mdl_sum_excl) %>%
    inner_join(booklet_design, by = c('test_id','booklet_id','module_nbr')) %>%
    inner_join(mdl_max_scores, by=c('test_id','miid')) %>%
    group_by(.data$routing) %>%
    do({
      if(.[['routing']][1] == 'all')
      {
        group_by(., .data$test_id, .data$biid) %>%
          arrange(.data$module_nbr) %>%
          mutate(module_exit_score_min = pmax(.data$module_exit_score_min - cumsum(.data$mdl_sum_excl),0),
                 module_exit_score_max = .data$module_exit_score_max - cumsum(.data$mdl_sum_excl)) %>%
          mutate(module_exit_score_max = lag_mod_max(.data$module_exit_score_max, .data$mod_max)) %>%
          ungroup()
      } else
      {
        mutate(., 
               module_exit_score_min = pmax(.data$module_exit_score_min - .data$mdl_sum_excl,0),
               module_exit_score_max = pmin(.data$module_exit_score_max - .data$mdl_sum_excl, .data$mod_max))
      }
    }) %>%
    ungroup() %>%
    arrange(.data$test_id, .data$biid, .data$module_nbr)
  
  
  #print(as.data.frame(routing %>% inner_join(check,by=c('booklet_id','biid','miid','module_nbr')) %>% arrange(biid, module_nbr )))
  

  booklets = lapply(split(booklet_items, paste(booklet_items$test_id, booklet_items$biid,sep='.')),
                    function(b)
                    {
                      route = filter(routing, .data$test_id == b$test_id[1] & .data$biid == b$biid[1])
                      
                      stab = scoretab %>% 
                        filter(.data$biid == b$biid[1] & .data$test_id == b$test_id[1] ) %>%
                        right_join(tibble(booklet_score = 0:sum(route$mod_max)), by = 'booklet_score') %>%
                        mutate(n = coalesce(n, 0L)) %>%
                        arrange(.data$booklet_score) %>%
                        pull(.data$n)
                      

                      list(booklet_id = paste(b$test_id[1], b$biid[1], sep = '.'),
                           modules = pull(route, .data$miid),
                           test_id = b$test_id[1],
                           min_scores = pull(route, .data$module_exit_score_min),
                           max_scores = pull(route, .data$module_exit_score_max),
                           routing = pull(route, .data$routing)[1],
                           items = pull(b, .data$item_id),
                           scoretab = stab,
                           first = split(pull(b, .data$first), pull(b, .data$module_nbr)),
                           last = split(pull(b, .data$last), pull(b, .data$module_nbr)))
                    })
  
  list(bkList = booklets, ssI = ssI, ssIS = ssIS, module_design_history = module_design_history, 
       plt=plt,
       module_design = semi_join(module_design, routing, by=c('test_id','miid')) %>% rename(module_id = 'miid'),
       booklet_design = select(booklet_items, .data$test_id, booklet_id = .data$biid, .data$item_id))
}  
