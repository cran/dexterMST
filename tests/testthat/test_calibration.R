context('enter sim data and test cml')

library(dplyr)


load('../data_sim.RData')

get_sim_all = function()
{
  
  
  scoring_rules<-data.frame(item_id=rep(paste0("item",sprintf("%02.0f",1:70)), each=2),
                            response=rep(0:1,times=70),
                            item_score=rep(0:1,times=70))
  
  design<-data.frame(item_id=paste0("item",sprintf("%02.0f",1:70)),
                     module_id=rep(c('M4','M2','M5','M1','M6','M3', 'M7'),times=rep(10,7)),
                     item_position=rep(1:10,7))
  
  db = create_mst_project(":memory:")
  add_scoring_rules_mst(db, scoring_rules)
  
  add_item_properties_mst(db,data.frame(item_id=colnames(data)[1:ncol(data)-1], delta=delta))
  
  
  routing_rules = mst_rules(
    '124' = M1[0:5] --+ M2[0:10] --+ M4, 
    '125' = M1[0:5] --+ M2[11:20] --+ M5,
    '136' = M1[6:10] --+ M3[6:15] --+ M6,
    '137' = M1[6:10] --+ M3[16:20] --+ M7)
  
  create_mst_test(db,
                  test_design = design,
                  routing_rules = routing_rules,
                  test_id = 'RU',
                  routing = "all")
  
  subset(data,((rowSums(data[,31:40])<=5)&(rowSums(data[,c(31:40,11:20)])<=10)),
                select=c(item31:item40, item11:item20, item01:item10, person_id)) %>%
    add_booklet_mst(db,.,booklet_id='124',test_id='RU')
  
  subset(data,((rowSums(data[,31:40])<=5)&(rowSums(data[,c(31:40,11:20)])>10)),
                     select=c(item31:item40, item11:item20, item21:item30,person_id)) %>%
    add_booklet_mst(db,.,booklet_id='125',test_id='RU')
  
  subset(data,((rowSums(data[,31:40])>5)&(rowSums(data[,c(31:40,51:60)])<=15)) ,
                     select=c(item31:item40,item51:item60, item41:item50, person_id)) %>%
    add_booklet_mst(db,.,booklet_id='136',test_id='RU')
 
  subset(data,((rowSums(data[,31:40])>5)&(rowSums(data[,c(31:40,51:60)])>15)) ,
                     select=c(item31:item40, item51:item60, item61:item70, person_id)) %>%
     add_booklet_mst(db,.,booklet_id='137',test_id='RU')
  
  add_person_properties_mst(db,data.frame(person_id=data$person_id, theta=theta))
  
  db
}


get_sim_last = function()
{
  scoring_rules<-data.frame(item_id=rep(paste0("item",sprintf("%02.0f",1:70)), each=2),
                            response=rep(0:1,times=70),
                            item_score=rep(0:1,times=70))
  
  design<-data.frame(item_id=paste0("item",sprintf("%02.0f",1:70)),
                     module_id=rep(c('M4','M2','M5','M1','M6','M3', 'M7'),times=rep(10,7)),
                     item_position=rep(1:10,7))
  
  routing_rules = mst_rules(
  '124' = M1[0:5] --+ M2[0:5] --+ M4, 
  '125' = M1[0:5] --+ M2[6:10] --+ M5,
  '136' = M1[6:10] --+ M3[0:5] --+ M6,
  '137' = M1[6:10] --+ M3[6:10] --+ M7)
  
  db = create_mst_project(":memory:")
  add_scoring_rules_mst(db, scoring_rules)
  add_item_properties_mst(db,data.frame(item_id=colnames(data)[1:ncol(data)-1], delta=delta))
  
  create_mst_test(db,
                  test_design = design,
                  routing_rules = routing_rules,
                  test_id = 'RU',
                  routing = "last")
  
  subset(data,((rowSums(data[,31:40])<=5)&(rowSums(data[,c(11:20)])<=5)),
                     select=c(item31:item40, item11:item20, item01:item10, person_id)) %>%
    add_booklet_mst(db,.,booklet_id='124',test_id='RU')
  
  subset(data,((rowSums(data[,31:40])<=5)&(rowSums(data[,c(11:20)])>5)),
                     select=c(item31:item40, item11:item20, item21:item30, person_id)) %>%
    add_booklet_mst(db,.,booklet_id='125',test_id='RU')
  
  subset(data,((rowSums(data[,31:40])>5)&(rowSums(data[,c(51:60)])<=5)) ,
                     select=c(item31:item40,item51:item60, item41:item50, person_id)) %>%
    add_booklet_mst(db,.,booklet_id='136',test_id='RU')
 
  subset(data,((rowSums(data[,31:40])>5)&(rowSums(data[,c(51:60)])>5)) ,
                     select=c(item31:item40, item51:item60, item61:item70, person_id)) %>%
    add_booklet_mst(db,.,booklet_id='137',test_id='RU')

  db
}

test_that('we can calibrate', {
  all_db = get_sim_all()
  last_db = get_sim_last()
  
  # all/last lead to same results
  
  fall = fit_enorm_mst(all_db)
  flast = fit_enorm_mst(last_db)

  expect_lt(mean(abs(coef(fall)$beta - coef(flast)$beta)),
            mean(coef(flast)$SE_b+coef(fall)$SE_b)/2,
            'mean difference all<->last < mean se')
  
  # close to true item parameters
  
  get_items_mst(all_db) %>%
    inner_join(coef(fall), by='item_id') %>%
    summarise(d = mean(abs(delta - beta)), se=mean(SE_beta)) %>%
    do({expect_lt(pull(.,d),pull(.,se), 'calibration delta close to true delta');.})        
  
  # predicates
  
  fall1 = fit_enorm_mst(all_db, item_id!='item32')
  flast1 = fit_enorm_mst(last_db, item_id!='item32')
  

  coef(fall1) %>%
    inner_join(coef(fall), by=c('item_id', 'item_score')) %>%
    mutate(d=abs(beta.x-beta.y)) %>%
    pull(d) %>%
    mean() %>%
    expect_lt(.01, 'all routing, omit item without problems')
  
  coef(flast1) %>%
    inner_join(coef(flast), by=c('item_id', 'item_score')) %>%
    mutate(d=abs(beta.x-beta.y)) %>%
    pull(d) %>%
    mean() %>%
    expect_lt(.01, 'last routing, omit item without problems')
  
 
  
  est_theta = ability(get_responses_mst(all_db), flast, method='EAP',prior='Jeffreys') %>%
    arrange(as.integer(person_id)) %>%
    pull(theta)
  

  
  expect_gt(cor(theta,est_theta), 0.9, 'estimate ability back')
  
  close_mst_project(all_db)
  close_mst_project(last_db)
  
})


