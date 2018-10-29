context('input checks')
library(dplyr)
library(dexter)
library(RSQLite)

df_equal = function(a,b, keys=NULL)
{
  if(!setequal(colnames(a), colnames(b))) return(FALSE)
  if(nrow(a) != nrow(b)) return(FALSE)
  
  b = b[,colnames(a)] 
  if(!is.null(keys))
  {
    b = arrange_at(b, keys)
    a = arrange_at(a, keys)
  }

  isTRUE(all.equal(as.data.frame(a),as.data.frame(b)))
}


test_that('mst_rules identifies basic syntax errors and routing errors', {
  expect_error({ mst_rules(hh = start[11:13] --+ m5[19:Inf] ---+ m6)}, 
                regexp = "expected `+`, found: -", fixed=TRUE)
  
  expect_error({mst_rules(ll = start[0:NA] --+ m2[0:10] --+ m1)},
               regexp = "maximum routing value should be an integer or Inf", fixed=TRUE)
               
  expect_error({mst_rules(ll = start[yolo:3] --+ m2[0:10] --+ m1)},
               regexp = "minimum routing value has to be an integer larger than zero", fixed=TRUE)
  
  
  # these overlap causing indeterminate routing
  # mm = start[7:10] --+ m3[14:17] --+ m4,
  # mh = start[7:10] --+ m3[17:Inf] --+ m6,
  expect_error({mst_rules(ll = start[0:6] --+ m2[0:10]   --+ m1,
                      lm = start[0:6] --+ m2[11:15]  --+ m4,
                      lh = start[0:6] --+ m2[16:Inf] --+ m6,
                      
                      ml = start[7:10] --+ m3[7:13] --+ m1,
                      mm = start[7:10] --+ m3[14:17] --+ m4,
                      mh = start[7:10] --+ m3[17:Inf] --+ m6,
                      
                      hl = start[11:13] --+ m5[11:13] --+ m1,
                      hm = start[11:13] --+ m5[14:18] --+ m4,
                      hh = start[11:13] --+ m5[19:Inf] --+ m6)},
               regexp='overlapping routing')
})



test_that('can import from dexter and calbration comparable to dexter', {
  dxdb = start_new_project(verbAggrRules, ":memory:", 
                                person_properties = list(gender = "unknown"))
  
  add_booklet(dxdb, verbAggrData, "agg") 
  add_item_properties(dxdb, verbAggrProperties)
  
  db = create_mst_project(':memory:')
  
  expect_output({import_from_dexter(db, dxdb)},'imported 7584 responses from 316 persons')
  
  
  expect_true(df_equal(get_items(dxdb),get_items_mst(db), keys='item_id'))


  fdx = fit_enorm(dxdb)
  fmst = fit_enorm_mst(db)
  
  coef_dx = coef(fdx)
  # dexter < 0.8.2
  if('SE_b' %in% colnames(coef_dx))
  {
    coef_dx = rename(coef_dx, SE_beta='SE_b')
  }
  
  expect_true(df_equal(coef(fmst), coef_dx, keys=c('item_id','item_score')),
              'dexter and dextermst coefficients equivalent')
  
  ddx = DIF(dxdb, 'gender')
  dmst = DIF_mst(db, 'gender')
  
  expect_equal(ddx$DIF_overall, dmst$DIF_overall, tolerance=1e-5)
  
  profile_tables_mst(fmst, get_items_mst(db),'situation') %>%
    select(-test_id) %>%                   
    df_equal(profile_tables(fdx, get_items_mst(db),'situation'), keys=c('sumScore','situation') ) %>%
    expect_true('dexter and dexterMST profile tables equivalent')
  
  
  dbDisconnect(db)
  dbDisconnect(dxdb)
  
})

