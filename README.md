<!-- README.md is generated from README.Rmd. Please edit that file -->
DexterMST
=========

DexterMST is an R package acting as a companion to dexter and adding facilities to manage and analyze data from multistage tests (MST). It includes functions for importing and managing test data, assessing and improving the quality of data through basic test and item analysis, and fitting an IRT model, all adapted to the peculiarities of MST designs. DexterMST typically works with project database files saved on disk.

Installation
------------

``` r
install.packages('dexterMST')
```

If you encounter a bug, please post a minimal reproducible example on [github](https://github.com/jessekps/dexter/issues). We post news and examples on a [blog](http://dexterities.netlify.com), it's also the place for general questions.

Example
-------

Here is an example for a simple two-stage test.

``` r
library(dexterMST)
library(dplyr)
# start a project
db = create_mst_project(":memory:")

# define dummy item scoring rules (i.e. response==score)
scoring_rules = data.frame(item_id = rep(sprintf("item%02.0f",1:50), each=2),
                            response = rep(0:1,times=50),
                            item_score = rep(0:1,times=50))

add_scoring_rules_mst(db, scoring_rules)

# define routing rules
routing_rules = mst_rules(
  easy = Mod_1[0:5] --+ Mod_2, 
  hard = Mod_1[6:10] --+ Mod_3)

# define a module design (i.e., specifify which items belong to which modules)
design = data.frame(module_id = rep(c('Mod_2','Mod_1','Mod_3'), times=c(20,10,20)),
                   item_id = paste0("item",sprintf("%02.0f",1:50)),
                   item_position = c(1:20,1:10,1:20))

# create/define an mst test
create_mst_test(db,
                test_design = design,
                routing_rules = routing_rules,
                test_id = 'ZwitserMaris')
```

We can now plot the design

``` r
# plot test designs for all tests in the project
design_plot(db)
```

We now simulate data:

``` r
sim_RM = function(theta,delta)
{
  nP=length(theta)
  dat=matrix(0,nP,length(delta))
  for (i in 1:length(delta)) dat[,i]=1*(rlogis(nP,0,1)<=(theta-delta[i]))
  return(dat)
}
a = rep(1,50)
delta = c(runif(20,-2.3,0),runif(10,-0.6,2),runif(20,1.2,2.4)) # M2, M1, M3
b=exp(-delta)
c = rep(0,50)
nP = 10000
# simulate theta from a mixture of two normals
grp = sample(2, nP, replace = TRUE, prob = c(.6,.4))
theta = rnorm(nP, mean = c(0,1)[grp], sd = c(1.5,0.5)[grp])

data = data.frame(sim_RM(theta, delta))
colnames(data) = sprintf("item%02.0f",1:50)

# add person id to the data
data$person_id = 1:nrow(data)

# extract two booklets from the complete data, based on the sum score on the first module
bk1 = data[rowSums(data[,21:30])<=5,] %>% select(person_id, item01:item30)
bk2 = data[rowSums(data[,21:30])>5,] %>% select(person_id, item21:item30, item31:item50)

# add response data to the project
add_booklet_mst(db, bk1, test_id = 'ZwitserMaris', booklet_id = 'easy')
add_booklet_mst(db, bk2, test_id = 'ZwitserMaris', booklet_id = 'hard')
```

``` r
# IRT, extended nominal response model
f = fit_enorm_mst(db)

head(f)
```

| item\_id |  item\_score|        beta|   SE\_beta|
|:---------|------------:|-----------:|----------:|
| item01   |            1|  -0.9378481|  0.0306780|
| item02   |            1|  -1.8104684|  0.0335670|
| item03   |            1|  -1.4555312|  0.0320611|
| item04   |            1|  -1.4145089|  0.0319168|
| item05   |            1|  -2.1264352|  0.0353192|
| item06   |            1|  -1.7498811|  0.0332764|

``` r
# ability estimates per person
rsp_data = get_responses_mst(db)
abl = ability(rsp_data, parms = f)
head(abl)
```

| booklet\_id | person\_id |  sumScore|       theta|
|:------------|:-----------|---------:|-----------:|
| easy        | 1          |         9|  -1.0288607|
| easy        | 10         |        23|   1.5102358|
| easy        | 100        |        23|   1.5102358|
| easy        | 1000       |         7|  -1.4308794|
| easy        | 10000      |        16|   0.1809514|
| easy        | 1001       |        13|  -0.3232153|

``` r
# ability estimates without item Item01
abl2 = ability(rsp_data, parms = f, item_id != "item01")

# plausible values
pv = plausible_values(rsp_data, parms = f, nPV = 5)
head(pv)
```

| booklet\_id | person\_id |  sumScore|         PV1|         PV2|         PV3|         PV4|         PV5|
|:------------|:-----------|---------:|-----------:|-----------:|-----------:|-----------:|-----------:|
| easy        | 1          |         9|  -0.4409709|  -0.2709257|  -0.8330791|  -1.0715047|  -0.4009712|
| easy        | 10         |        23|   1.5483993|   1.0318135|   1.3107129|   0.4420585|   1.6680941|
| easy        | 100        |        23|   1.7134969|   1.5736628|   1.5000658|   1.8495170|   0.3622951|
| easy        | 1000       |         7|  -1.0344610|  -1.7829133|  -1.3854931|  -0.7899523|  -1.4050426|
| easy        | 10000      |        16|   0.0497648|   0.1811482|  -0.1966962|   0.5819318|   0.4640787|
| easy        | 1001       |        13|  -0.0745779|  -0.8048086|   0.2805427|   0.4067189|  -0.2027418|

Contributing
------------

Contributions are welcome but please check with us first about what you would like to contribute.
