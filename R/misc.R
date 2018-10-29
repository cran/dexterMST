
.onUnload <- function (libpath) {
  library.dynam.unload("dexterMST", libpath)
}



# #' @export
# dexter::ability


# #' @export
# dexter::ability_tables


# #' @export
# dexter::plausible_values


# mix default with user args
merge_arglists = function(args, default = NULL, override = NULL)
{
  if(is.null(default))
    default = list()
  
  if(is.null(override))
    override = list()
  
  res = modifyList(default, args)
  res = modifyList(res, override)
  res
}



# given min and max values defining a number of ranges
# returns a logical vector indicating whether each range overlaps 
# with any others in the set
#
range_overlap = function(mn, mx)
{
  stopifnot(length(mn) == length(mx))
  stopifnot(all(mx >= mn))
  if(length(mn) == 1) return(FALSE)
  
  combinations = as.data.frame(combn(1:length(mn), 2))
  
  overlap = unlist(lapply(combinations, function(i)
  {
    between(mn[i[1]], mn[i[2]], mx[i[2]]) || 
      between(mx[i[1]], mn[i[2]], mx[i[2]]) ||
      between(mx[i[2]], mn[i[1]], mx[i[1]])
  }))
  
  ovl = combinations[overlap] %>% unlist() %>% unique()
  
  vapply(1:length(mn), function(i){ i %in% ovl}, FALSE)
}


# check a single rule in mst rules for syntactic validity
check_rule = function(rule)
{
  
  if(names(rule)[length(rule)] != 'sym')
    stop(paste('error in rule:\n',paste(rule, collapse=''),'rule should end with a module name'))
  
  mod_name = function(ch) if(names(ch) != 'sym') 'unexpected operator'
  lbracket = function(ch) if(names(ch) != 'op' || ch != '[') paste('expected `[`, found:', ch)
  rbracket = function(ch) if(names(ch) != 'op' || ch != ']') paste('expected `]`, found:', ch)
  col = function(ch) if(names(ch) != 'op' || ch != ':') paste('expected `:`, found:', ch)
  minus = function(ch) if(names(ch) != 'op' || ch != '-') paste('expected `-`, found:', ch)
  plus = function(ch) if(names(ch) != 'op' || ch != '+') paste('expected `+`, found:', ch)
  minrt = function(ch)
  {
    if(!(names(ch) == 'sym' && grepl('^\\d+$', ch, perl=TRUE))) 
      'minimum routing value has to be an integer larger than zero'
  }
  maxrt = function(ch)
  {
    if(!(names(ch) == 'sym' && grepl('^(Inf)|(\\d+)$', ch, perl=TRUE))) 
      'maximum routing value should be an integer or Inf'
  }
  
  
  check = rep_len(list(mod_name, lbracket, minrt, col, maxrt, rbracket, minus, minus, plus), length(rule))
  
  for(i in seq_along(rule))
  {
    msg = check[[i]](rule[i])
    if(!is.null(msg))
    {
      indx = sum(sapply(rule[1:i-1],nchar)) -1
      msg = paste('error in rule:\n',
                  paste(rule, collapse=''),'\n', 
                  paste(rep_len(' ',indx), collapse=''),'^\n',
                  msg)
      stop(msg)
    }
  }
  rule
}

# given routing_rules as returned by mst_rules
# checks the validty (without regard to items)
# returns nothing, called for side effect of throwing an error
#
# might work for last routing as well as all, to test
check_routing = function(routing_rules)
{
  fmod = factor(routing_rules$module_id)
  routing_rules$miid = match(fmod, routing_rules$module_id)
  
  path_collision = routing_rules %>%
    mutate(exit_min = coalesce(.data$exit_min,0L), 
           exit_max = coalesce(.data$exit_max, .Machine$integer.max)) %>%
    group_by(.data$booklet_id) %>%
    mutate(path = paste(Reduce(paste, .data$miid, accumulate = TRUE)),
           nextmod = lead(.data$module_id)) %>%
    ungroup() %>%
    group_by(.data$path, .data$nextmod) %>%
    do({
      d = .
      if(nrow(distinct(d, .data$exit_min, .data$exit_max)) > 1)
        stop(paste0("multiple routes from module '", unique(d$module_id), "' to module '", unique(d$nextmod), "'"))
      slice(d, 1)
    }) %>%
    ungroup() %>%
    group_by(.data$path) %>%
    filter(range_overlap(.data$exit_min, .data$exit_max))
  
  if(nrow(path_collision) > 0)
  {
    stop(paste('booklets', paste0("'", path_collision$booklet_id, "'", collapse=','),
               'have overlapping routing rules'))
  }
  NULL
}

