# ===========================================================
#
# Q: What are the advantages to live in a volcanic risk area?
#
# ___________________________________________________________

    # This study aims to 
    #     - identify the perceptual advantages held by communities on Mount Semeru, 
    #     - measure their association with residencies in high-risk zones, 
    #     - and analyze gender’s moderating role, 
    # testing the hypothesis that these advantages offset objective risk in hazardous-area residency..

    #
    # ---- load data ----
    #

# raw data..
advantages <- read.csv( "data/advantages.csv" )
    # see
    str(advantages)
## 'data.frame':   753 obs. of  3 variables:
##  $ Risky_Area          : chr  "Risky" "Risky" "Risky" "Risky" ...
##  $ gender              : chr  "Male" "Male" "Female" "Female" ...
##  $ advantage_risky_area: chr  "tanah subur" "tanah subur, mata air besar" "Jauh dari polusi  Gampang Mandapat bahan makanan" "Bisa makan, dapat pangasilan,dekat sama keluarga" ...

    #
    # ---- categories of advantages ----
    # 
# item responses [`sda`..`pasir`] are categorized into 6 frameworks..
# each item in a framework is identified in a written response
# see `wordcloud.R` for item responses..
# this would be `Y` matrix of `WYZ`

    NAT_RES = "sda|pasir|tambang|batu"
    LAND = "subur|tanah|lahan|air"
    ECONOMIC = "ekonomi|kerja|pencaharian|makan|rezeki|wisata|tanaman|pangan|tani|kebun"
    BIRTH = "lahir|parent|keluarga|choice|rukun|gotong"
    COMFORT = "safe|comfort|tentram|asri|tenang" 
    CLIMATE = "udara|sejuk|segar|dingin|polusi|cuaca|adem"

    #
    # ---- MRCV analysis ---
    #
# see Koziol and Bilder 2014
#     "Multiple Response Categorical Variables: 
#       A Log-Linear Modeling Approach"
#
### install.packages("MRCV")

# generate item response table as `WYZ` matrix..
    # SRCV == `W` matrix..2 states..`Safe` or `Risky`..
    # MRCV == `Y` matrix..6 conceptual frameworks..
    # SRCV == `Z` matrix..2 states..`Male` or `Female`..

# ---- .1 `W` matrix ----
    #
    # the first matrix of the `WYZ` item response table..
    # this should be an SCRV..SINGLE response categorical variable..
    #    the area of residence..`Safe` or `Risky`..
    #    participants were assigned to one of the two areas..
    # this is a fixed variable [in the mixed effects jargon]..
#
    # living in `Safe` == 1 or `Risky` == 2 areas..
    #   + by reason unknown to me..`MRCV::genloglin()` refused to
    #   + work with SRCV[1/2] as `W` matrix..
    #   + but okay with MRCV[0+1]..so this is used troughout..
    #   + i checked `WY` using SRCV[1/2] & MRCV[0+1]
    #   + vs. MRCV[0+1] & MRCV[0+1] ..
    #   + the results are exactly same..
    #   + now let's go to `WYZ` analysis..
    advant <- data.frame( SAFE=rep(0,nrow(advantages)) ,    # initialize
                          RISKY=rep(0,nrow(advantages)) )   #
    # `0` denotes `FALSE`..`1` denotes `TRUE`
advant$SAFE <- ifelse( advantages$Risky_Area=="Safe", 1 , 0 )   # disjunctive 
advant$RISKY <- ifelse( advant$SAFE==1 , 0 , 1 )                # matrix
    # see
    str(advant)
## 'data.frame':   753 obs. of  2 variables:
##  $ SAFE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ RISKY: num  1 1 1 1 1 1 1 1 1 1 ...
#
# ---- .2 `Y` matrix ----
    #
    # the second martrix of the `WYZ`..
    # this is a MULTIPLE response categorical variable..
    #   consisting of item responses by the participants..
    # items are grouped into 6 frameworks..
    #
    # `0` denotes item in a framework not matched to the original written answers..
    # `1` denotes item matched to the original written answers..
    # we use `base::grepl()` to identify the match..
    elements <- c(NAT_RES, LAND, ECONOMIC, BIRTH, COMFORT, CLIMATE)
    for (element in elements) {
advant[,element] <- ifelse( grepl( element , 
                                    advantages$advantage_risky_area,
                                    ignore.case = TRUE, fixed = FALSE), 
                                    1, 0)
                          }
    str(advant)
## 'data.frame':   753 obs. of  8 variables:
##  $ SAFE                                                                   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ RISKY                                                                  : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ sda|pasir|tambang|batu                                                 : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ subur|tanah|lahan|air                                                  : num  1 1 0 0 0 1 1 1 0 0 ...
##  $ ekonomi|kerja|pencaharian|makan|rezeki|wisata|tanaman|pangan|tani|kebun: num  0 0 1 1 1 0 0 0 0 1 ...
##  $ lahir|parent|keluarga|choice|rukun|gotong                              : num  0 0 0 1 1 0 0 0 0 0 ...
##  $ safe|comfort|tentram|asri|tenang                                       : num  0 0 0 0 0 0 0 0 0 1 ...
##  $ udara|sejuk|segar|dingin|polusi|cuaca|adem                             : num  0 0 1 0 0 1 1 1 1 0 ...


# ---- .3 `Z` matrix ----
    #
    # the third matrix of the `WYZ` item response table..
    # this should be an SCRV..SINGLE response categorical variable..
    #    gender of respondents..`Female` or `Male`..
    # this has same class with `W` matrix
advant$MALE <- ifelse( advantages$gender=="Male", 1 , 0 )
advant$FEMALE <- ifelse( advant$MALE==1, 0 , 1 )
#
    names(advant) <- c("SAFE","RISKY",
        "NAT_RES", "LAND", "ECONOMIC", "BIRTH", "COMFORT", "CLIMATE",
        "MALE", "FEMALE")
    str(advant)
## 'data.frame':   753 obs. of  10 variables:
## $ SAFE    : num  0 0 0 0 0 0 0 0 0 0 ...
## $ RISKY   : num  1 1 1 1 1 1 1 1 1 1 ...
## $ NAT_RES : num  0 0 0 0 0 0 0 0 0 0 ...
## $ LAND    : num  1 1 0 0 0 1 1 1 0 0 ...
## $ ECONOMIC: num  0 0 1 1 1 0 0 0 0 1 ...
## $ BIRTH   : num  0 0 0 1 1 0 0 0 0 0 ...
## $ COMFORT : num  0 0 0 0 0 0 0 0 0 1 ...
## $ CLIMATE : num  0 0 1 0 0 1 1 1 1 0 ...
## $ MALE    : num  1 1 0 0 0 1 0 1 0 0 ...
## $ FEMALE  : num  0 0 1 1 1 0 1 0 1 1 ...

    #
    # ordinary contingency table
    #
#
    MALEinRISKY <- advant[advant$RISKY=="1"&advant$MALE=="1",3:8]
    MALEinSAFE <- advant[advant$SAFE=="1"&advant$MALE=="1",3:8]
    FEMALEinRISKY <- advant[advant$RISKY=="1"&advant$FEMALE=="1",3:8]
    FEMALEinSAFE <- advant[advant$SAFE=="1"&advant$FEMALE=="1",3:8]
advant.tbl <- data.frame(
    apply(MALEinRISKY,2,sum) |> 
        as.matrix() |> 
        as.data.frame() ,
    apply(MALEinSAFE,2,sum) |> 
        as.matrix() |> 
        as.data.frame() ,
    apply(FEMALEinRISKY,2,sum) |> 
        as.matrix() |> 
        as.data.frame() ,
    apply(FEMALEinSAFE,2,sum) |> 
        as.matrix() |> 
        as.data.frame()
    )
    names(advant.tbl) <- c( "MALEinRISKY" , "MALEinSAFE" ,
                            "FEMALEinRISKY" , "FEMALEinSAFE" )
    advant.tbl      # see
##          MALEinRISKY MALEinSAFE FEMALEinRISKY FEMALEinSAFE
## NAT_RES           12         17             9           34
## LAND              93        154            79          163
## ECONOMIC          63         84            56           69
## BIRTH             12          5            16           11
## COMFORT           17         19            16           24
## CLIMATE           51        121            58          145
    #
    # check..! sum is wrong..as expected..
    apply(advant.tbl,2,sum)
##   MALEinRISKY    MALEinSAFE FEMALEinRISKY  FEMALEinSAFE 
##           248           400           234           446 

    #
    # ---- `MRCV::genloglin()` analysis ----
    #

    library(MRCV)
    #
    # `genloglin()` function does not accept the `y.main` formula..
    # need to write the formula ourselves..
    # see `farmer3` data in the package..
#
    # rewrite column names..
    names(advant) <- c("w1" , "w2" , 
                       "y1" , "y2" , "y3" , "y4" , "y5" , "y6" , 
                       "z1" , "z2")
    str(advant)
## 'data.frame':   753 obs. of  10 variables:
##  $ w1: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ w2: num  1 1 1 1 1 1 1 1 1 1 ...
##  $ y1: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ y2: num  1 1 0 0 0 1 1 1 0 0 ...
##  $ y3: num  0 0 1 1 1 0 0 0 0 1 ...
##  $ y4: num  0 0 0 1 1 0 0 0 0 0 ...
##  $ y5: num  0 0 0 0 0 0 0 0 0 1 ...
##  $ y6: num  0 0 1 0 0 1 1 1 1 0 ...
##  $ z1: num  1 1 0 0 0 1 0 1 0 0 ...
##  $ z2: num  0 0 1 1 1 0 1 0 1 1 ...
    #
    set.seed(1)       # in case of bootstrap
mod.fit <- MRCV::genloglin( data = advant, I = 2, J = 6, K = 2,
    model = count ~ -1 + 
    W:Y:Z + 
    wi%in%W:Y:Z + yj%in%W:Y:Z + zk%in%W:Y:Z + 
    wi:yj +  wi:yj%in%Y + wi:yj%in%W + wi:yj%in%Y:W + 
    yj:zk +  yj:zk%in%Z + yj:zk%in%Y + yj:zk%in%Z:Y, 
    boot = TRUE, B = 999)
    summary(mod.fit)
## Call:
## "glm(formula = count ~ -1 + W:Y:Z + wi %in% W:Y:Z + yj %in% W:Y:Z + zk %in% W:Y:Z + wi:yj + wi:yj %in% Y + wi:yj %in% W + wi:yj %in% Y:W + yj:zk + yj:zk %in% Z + yj:zk %in% Y + yj:zk %in% Z:Y , family = poisson(link = log), data = model.data)"
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.05374  -0.29303   0.00163   0.29893   1.14489  
## 
## Coefficients:
##                Estimate    RS SE z value Pr(>|z|)    
## wi:yj           0.25954  0.27147   0.956 0.339056    
## yj:zk          -0.48501  0.25224  -1.923 0.054503 .  
## Ww1:Yy1:Zz1     4.72832  0.06687  70.713  < 2e-16 ***
## Ww2:Yy1:Zz1     5.35609  0.05048 106.107  < 2e-16 ***
## Ww1:Yy2:Zz1     3.71468  0.12093  30.717  < 2e-16 ***
## Ww2:Yy2:Zz1     4.44212  0.09140  48.602  < 2e-16 ***
## Ww1:Yy3:Zz1     4.25167  0.08714  48.792  < 2e-16 ***
## Ww2:Yy3:Zz1     5.15200  0.06130  84.044  < 2e-16 ***
## Ww1:Yy4:Zz1     4.70611  0.06632  70.959  < 2e-16 ***
## Ww2:Yy4:Zz1     5.43973  0.04848 112.199  < 2e-16 ***
## Ww1:Yy5:Zz1     4.69144  0.06767  69.327  < 2e-16 ***
## Ww2:Yy5:Zz1     5.38902  0.05010 107.562  < 2e-16 ***
## Ww1:Yy6:Zz1     4.17500  0.09291  44.935  < 2e-16 ***
## Ww2:Yy6:Zz1     4.60477  0.08101  56.845  < 2e-16 ***
## Ww1:Yy1:Zz2     4.81943  0.06525  73.856  < 2e-16 ***
## Ww2:Yy1:Zz2     5.44719  0.04744 114.816  < 2e-16 ***
## Ww1:Yy2:Zz2     3.80565  0.11661  32.636  < 2e-16 ***
## Ww2:Yy2:Zz2     4.53309  0.08847  51.241  < 2e-16 ***
## Ww1:Yy3:Zz2     4.23088  0.09113  46.427  < 2e-16 ***
## Ww2:Yy3:Zz2     5.13121  0.05992  85.636  < 2e-16 ***
## Ww1:Yy4:Zz2     4.78231  0.06678  71.613  < 2e-16 ***
## Ww2:Yy4:Zz2     5.51593  0.04475 123.252  < 2e-16 ***
## Ww1:Yy5:Zz2     4.75350  0.06787  70.035  < 2e-16 ***
## Ww2:Yy5:Zz2     5.45108  0.04714 115.647  < 2e-16 ***
## Ww1:Yy6:Zz2     4.43034  0.08696  50.950  < 2e-16 ***
## Ww2:Yy6:Zz2     4.86012  0.07074  68.702  < 2e-16 ***
## Yy2:wi:yj      -0.37557  0.33179  -1.132 0.257657    
## Yy3:wi:yj      -0.90856  0.32223  -2.820 0.004808 ** 
## Yy4:wi:yj      -1.55278  0.43871  -3.539 0.000401 ***
## Yy5:wi:yj      -0.69243  0.36809  -1.881 0.059952 .  
## Yy6:wi:yj       0.20283  0.33295   0.609 0.542385    
## Ww2:wi:yj      -0.51908  0.54295  -0.956 0.339056    
## Zz2:yj:zk       0.97002  0.50448   1.923 0.054503 .  
## Yy2:yj:zk       0.41449  0.31002   1.337 0.181234    
## Yy3:yj:zk       0.66792  0.30170   2.214 0.026838 *  
## Yy4:yj:zk      -0.05381  0.42149  -0.128 0.898405    
## Yy5:yj:zk       0.31759  0.35555   0.893 0.371729    
## Yy6:yj:zk       0.06395  0.31294   0.204 0.838075    
## Ww1:Yy1:Zz1:wi  0.62776  0.08045   7.803 6.00e-15 ***
## Ww2:Yy1:Zz1:wi -0.62776  0.08045  -7.803 6.00e-15 ***
## Ww1:Yy2:Zz1:wi  0.72744  0.13132   5.539 3.04e-08 ***
## Ww2:Yy2:Zz1:wi -0.72744  0.13132  -5.539 3.04e-08 ***
## Ww1:Yy3:Zz1:wi  0.90034  0.10059   8.951  < 2e-16 ***
## Ww2:Yy3:Zz1:wi -0.90034  0.10059  -8.951  < 2e-16 ***
## Ww1:Yy4:Zz1:wi  0.73362  0.08022   9.145  < 2e-16 ***
## Ww2:Yy4:Zz1:wi -0.73362  0.08022  -9.145  < 2e-16 ***
## Ww1:Yy5:Zz1:wi  0.69758  0.08159   8.550  < 2e-16 ***
## Ww2:Yy5:Zz1:wi -0.69758  0.08159  -8.550  < 2e-16 ***
## Ww1:Yy6:Zz1:wi  0.42978  0.10525   4.083 4.44e-05 ***
## Ww2:Yy6:Zz1:wi -0.42978  0.10525  -4.083 4.44e-05 ***
## Ww1:Yy1:Zz2:wi  0.62776  0.08045   7.803 6.00e-15 ***
## Ww2:Yy1:Zz2:wi -0.62776  0.08045  -7.803 6.00e-15 ***
## Ww1:Yy2:Zz2:wi  0.72744  0.13132   5.539 3.04e-08 ***
## Ww2:Yy2:Zz2:wi -0.72744  0.13132  -5.539 3.04e-08 ***
## Ww1:Yy3:Zz2:wi  0.90034  0.10059   8.951  < 2e-16 ***
## Ww2:Yy3:Zz2:wi -0.90034  0.10059  -8.951  < 2e-16 ***
## Ww1:Yy4:Zz2:wi  0.73362  0.08022   9.145  < 2e-16 ***
## Ww2:Yy4:Zz2:wi -0.73362  0.08022  -9.145  < 2e-16 ***
## Ww1:Yy5:Zz2:wi  0.69758  0.08159   8.550  < 2e-16 ***
## Ww2:Yy5:Zz2:wi -0.69758  0.08159  -8.550  < 2e-16 ***
## Ww1:Yy6:Zz2:wi  0.42978  0.10525   4.083 4.44e-05 ***
## Ww2:Yy6:Zz2:wi -0.42978  0.10525  -4.083 4.44e-05 ***
## Ww1:Yy1:Zz1:yj -2.19927  0.23439  -9.383  < 2e-16 ***
## Ww2:Yy1:Zz1:yj -1.93973  0.18995 -10.212  < 2e-16 ***
## Ww1:Yy2:Zz1:yj  0.72939  0.15349   4.752 2.01e-06 ***
## Ww2:Yy2:Zz1:yj  0.61336  0.12281   4.994 5.91e-07 ***
## Ww1:Yy3:Zz1:yj -0.25003  0.14773  -1.692 0.090554 .  
## Ww2:Yy3:Zz1:yj -0.89906  0.12573  -7.151 8.64e-13 ***
## Ww1:Yy4:Zz1:yj -1.86225  0.22940  -8.118 4.44e-16 ***
## Ww2:Yy4:Zz1:yj -3.15549  0.29319 -10.763  < 2e-16 ***
## Ww1:Yy5:Zz1:yj -1.83679  0.21443  -8.566  < 2e-16 ***
## Ww2:Yy5:Zz1:yj -2.26968  0.20131 -11.274  < 2e-16 ***
## Ww1:Yy6:Zz1:yj -0.09737  0.14596  -0.667 0.504700    
## Ww2:Yy6:Zz1:yj  0.36500  0.11809   3.091 0.001996 ** 
## Ww1:Yy1:Zz2:yj -2.68428  0.29228  -9.184  < 2e-16 ***
## Ww2:Yy1:Zz2:yj -2.42474  0.19746 -12.280  < 2e-16 ***
## Ww1:Yy2:Zz2:yj  0.65887  0.15176   4.342 1.42e-05 ***
## Ww2:Yy2:Zz2:yj  0.54284  0.11898   4.562 5.06e-06 ***
## Ww1:Yy3:Zz2:yj -0.06712  0.14496  -0.463 0.643321    
## Ww2:Yy3:Zz2:yj -0.71615  0.12123  -5.907 3.48e-09 ***
## Ww1:Yy4:Zz2:yj -2.40108  0.28734  -8.356  < 2e-16 ***
## Ww2:Yy4:Zz2:yj -3.69432  0.30535 -12.099  < 2e-16 ***
## Ww1:Yy5:Zz2:yj -2.00421  0.23057  -8.692  < 2e-16 ***
## Ww2:Yy5:Zz2:yj -2.43710  0.19914 -12.238  < 2e-16 ***
## Ww1:Yy6:Zz2:yj -0.51843  0.14601  -3.551 0.000384 ***
## Ww2:Yy6:Zz2:yj -0.05606  0.11487  -0.488 0.625560    
## Ww1:Yy1:Zz1:zk  0.09111  0.07672   1.188 0.235026    
## Ww2:Yy1:Zz1:zk  0.09111  0.07672   1.188 0.235026    
## Ww1:Yy2:Zz1:zk  0.09097  0.12322   0.738 0.460336    
## Ww2:Yy2:Zz1:zk  0.09097  0.12322   0.738 0.460336    
## Ww1:Yy3:Zz1:zk -0.02079  0.09120  -0.228 0.819665    
## Ww2:Yy3:Zz1:zk -0.02079  0.09120  -0.228 0.819665    
## Ww1:Yy4:Zz1:zk  0.07620  0.07517   1.014 0.310697    
## Ww2:Yy4:Zz1:zk  0.07620  0.07517   1.014 0.310697    
## Ww1:Yy5:Zz1:zk  0.06206  0.07690   0.807 0.419686    
## Ww2:Yy5:Zz1:zk  0.06206  0.07690   0.807 0.419686    
## Ww1:Yy6:Zz1:zk  0.25535  0.10371   2.462 0.013810 *  
## Ww2:Yy6:Zz1:zk  0.25535  0.10371   2.462 0.013810 *  
## Ww1:Yy1:Zz2:zk -0.09111  0.07672  -1.188 0.235026    
## Ww2:Yy1:Zz2:zk -0.09111  0.07672  -1.188 0.235026    
## Ww1:Yy2:Zz2:zk -0.09097  0.12322  -0.738 0.460336    
## Ww2:Yy2:Zz2:zk -0.09097  0.12322  -0.738 0.460336    
## Ww1:Yy3:Zz2:zk  0.02079  0.09120   0.228 0.819665    
## Ww2:Yy3:Zz2:zk  0.02079  0.09120   0.228 0.819665    
## Ww1:Yy4:Zz2:zk -0.07620  0.07517  -1.014 0.310697    
## Ww2:Yy4:Zz2:zk -0.07620  0.07517  -1.014 0.310697    
## Ww1:Yy5:Zz2:zk -0.06206  0.07690  -0.807 0.419686    
## Ww2:Yy5:Zz2:zk -0.06206  0.07690  -0.807 0.419686    
## Ww1:Yy6:Zz2:zk -0.25535  0.10371  -2.462 0.013810 *  
## Ww2:Yy6:Zz2:zk -0.25535  0.10371  -2.462 0.013810 *  
## Ww2:Yy2:wi:yj   0.75114  0.66358   1.132 0.257657    
## Ww2:Yy3:wi:yj   1.81712  0.64445   2.820 0.004808 ** 
## Ww2:Yy4:wi:yj   3.10555  0.87743   3.539 0.000401 ***
## Ww2:Yy5:wi:yj   1.38486  0.73618   1.881 0.059952 .  
## Ww2:Yy6:wi:yj  -0.40567  0.66589  -0.609 0.542385    
## Yy2:Zz2:yj:zk  -0.82898  0.62004  -1.337 0.181234    
## Yy3:Zz2:yj:zk  -1.33584  0.60340  -2.214 0.026838 *  
## Yy4:Zz2:yj:zk   0.10763  0.84298   0.128 0.898405    
## Yy5:Zz2:yj:zk  -0.63518  0.71110  -0.893 0.371729    
## Yy6:Zz2:yj:zk  -0.12790  0.62588  -0.204 0.838075    
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##         Null deviance: 138445.754    Residual deviance:     34.353
##         Number of Fisher Scoring iterations: 4
    #
    predict(mod.fit, pair = "WY")
## Bootstrap Progress: 
##   |======================================================================| 100%
## Observed odds ratios with 95% asymptotic confidence intervals 
##             OR lower.bound upper.bound
## z1=1,w1y1 0.77        0.35        1.66
## z1=1,w1y2 0.78        0.50        1.20
## z1=1,w1y3 0.60        0.39        0.92
## z1=1,w1y4 0.21        0.07        0.62
## z1=1,w1y5 0.59        0.29        1.17
## z1=1,w1y6 1.61        1.05        2.46
## z1=1,w2y1 1.30        0.60        2.82
## z1=1,w2y2 1.29        0.83        2.01
## z1=1,w2y3 1.66        1.08        2.55
## z1=1,w2y4 4.67        1.61       13.54
## z1=1,w2y5 1.71        0.86        3.41
## z1=1,w2y6 0.62        0.41        0.95
## z2=1,w1y1 1.99        0.92        4.29
## z2=1,w1y2 1.03        0.65        1.63
## z2=1,w1y3 0.45        0.29        0.71
## z2=1,w1y4 0.31        0.14        0.68
## z2=1,w1y5 0.71        0.36        1.39
## z2=1,w1y6 1.54        1.00        2.39
## z2=1,w2y1 0.50        0.23        1.09
## z2=1,w2y2 0.97        0.61        1.53
## z2=1,w2y3 2.22        1.41        3.50
## z2=1,w2y4 3.27        1.47        7.29
## z2=1,w2y5 1.42        0.72        2.78
## z2=1,w2y6 0.65        0.42        1.00
## z1=0,w1y1 1.99        0.92        4.29
## z1=0,w1y2 1.03        0.65        1.63
## z1=0,w1y3 0.45        0.29        0.71
## z1=0,w1y4 0.31        0.14        0.68
## z1=0,w1y5 0.71        0.36        1.39
## z1=0,w1y6 1.54        1.00        2.39
## z1=0,w2y1 0.50        0.23        1.09
## z1=0,w2y2 0.97        0.61        1.53
## z1=0,w2y3 2.22        1.41        3.50
## z1=0,w2y4 3.27        1.47        7.29
## z1=0,w2y5 1.42        0.72        2.78
## z1=0,w2y6 0.65        0.42        1.00
## z2=0,w1y1 0.77        0.35        1.66
## z2=0,w1y2 0.78        0.50        1.20
## z2=0,w1y3 0.60        0.39        0.92
## z2=0,w1y4 0.21        0.07        0.62
## z2=0,w1y5 0.59        0.29        1.17
## z2=0,w1y6 1.61        1.05        2.46
## z2=0,w2y1 1.30        0.60        2.82
## z2=0,w2y2 1.29        0.83        2.01
## z2=0,w2y3 1.66        1.08        2.55
## z2=0,w2y4 4.67        1.61       13.54
## z2=0,w2y5 1.71        0.86        3.41
## z2=0,w2y6 0.62        0.41        0.95
## 
## Model-predicted odds ratios with 95% asymptotic confidence intervals 
##             OR lower.bound upper.bound
## z1=1,w1y1 1.30        0.76        2.21
## z1=1,w1y2 0.89        0.65        1.22
## z1=1,w1y3 0.52        0.38        0.71
## z1=1,w1y4 0.27        0.15        0.52
## z1=1,w1y5 0.65        0.40        1.05
## z1=1,w1y6 1.59        1.17        2.15
## z1=1,w2y1 0.77        0.45        1.31
## z1=1,w2y2 1.12        0.82        1.54
## z1=1,w2y3 1.91        1.40        2.61
## z1=1,w2y4 3.64        1.93        6.87
## z1=1,w2y5 1.54        0.95        2.49
## z1=1,w2y6 0.63        0.46        0.85
## z2=1,w1y1 1.30        0.76        2.21
## z2=1,w1y2 0.89        0.65        1.22
## z2=1,w1y3 0.52        0.38        0.71
## z2=1,w1y4 0.27        0.15        0.52
## z2=1,w1y5 0.65        0.40        1.05
## z2=1,w1y6 1.59        1.17        2.15
## z2=1,w2y1 0.77        0.45        1.31
## z2=1,w2y2 1.12        0.82        1.54
## z2=1,w2y3 1.91        1.40        2.61
## z2=1,w2y4 3.64        1.93        6.87
## z2=1,w2y5 1.54        0.95        2.49
## z2=1,w2y6 0.63        0.46        0.85
## z1=0,w1y1 1.30        0.76        2.21
## z1=0,w1y2 0.89        0.65        1.22
## z1=0,w1y3 0.52        0.38        0.71
## z1=0,w1y4 0.27        0.15        0.52
## z1=0,w1y5 0.65        0.40        1.05
## z1=0,w1y6 1.59        1.17        2.15
## z1=0,w2y1 0.77        0.45        1.31
## z1=0,w2y2 1.12        0.82        1.54
## z1=0,w2y3 1.91        1.40        2.61
## z1=0,w2y4 3.64        1.93        6.87
## z1=0,w2y5 1.54        0.95        2.49
## z1=0,w2y6 0.63        0.46        0.85
## z2=0,w1y1 1.30        0.76        2.21
## z2=0,w1y2 0.89        0.65        1.22
## z2=0,w1y3 0.52        0.38        0.71
## z2=0,w1y4 0.27        0.15        0.52
## z2=0,w1y5 0.65        0.40        1.05
## z2=0,w1y6 1.59        1.17        2.15
## z2=0,w2y1 0.77        0.45        1.31
## z2=0,w2y2 1.12        0.82        1.54
## z2=0,w2y3 1.91        1.40        2.61
## z2=0,w2y4 3.64        1.93        6.87
## z2=0,w2y5 1.54        0.95        2.49
## z2=0,w2y6 0.63        0.46        0.85
## 
## Bootstrap Results: 
## Final results based on 999 resamples 
## Model-predicted odds ratios with 95% bootstrap BCa confidence intervals 
##             OR lower.bound upper.bound
## z1=1,w1y1 1.30        0.74        2.29
## z1=1,w1y2 0.89        0.64        1.20
## z1=1,w1y3 0.52        0.39        0.72
## z1=1,w1y4 0.27        0.13        0.51
## z1=1,w1y5 0.65        0.41        1.05
## z1=1,w1y6 1.59        1.16        2.21
## z1=1,w2y1 0.77        0.43        1.34
## z1=1,w2y2 1.12        0.83        1.55
## z1=1,w2y3 1.91        1.39        2.56
## z1=1,w2y4 3.64        1.93        7.35
## z1=1,w2y5 1.54        0.94        2.42
## z1=1,w2y6 0.63        0.45        0.86
## z2=1,w1y1 1.30        0.74        2.29
## z2=1,w1y2 0.89        0.64        1.20
## z2=1,w1y3 0.52        0.39        0.72
## z2=1,w1y4 0.27        0.13        0.51
## z2=1,w1y5 0.65        0.41        1.05
## z2=1,w1y6 1.59        1.16        2.21
## z2=1,w2y1 0.77        0.43        1.34
## z2=1,w2y2 1.12        0.83        1.55
## z2=1,w2y3 1.91        1.39        2.56
## z2=1,w2y4 3.64        1.93        7.35
## z2=1,w2y5 1.54        0.94        2.42
## z2=1,w2y6 0.63        0.45        0.86
## z1=0,w1y1 1.30        0.74        2.29
## z1=0,w1y2 0.89        0.64        1.20
## z1=0,w1y3 0.52        0.39        0.72
## z1=0,w1y4 0.27        0.13        0.51
## z1=0,w1y5 0.65        0.41        1.05
## z1=0,w1y6 1.59        1.16        2.21
## z1=0,w2y1 0.77        0.43        1.34
## z1=0,w2y2 1.12        0.83        1.55
## z1=0,w2y3 1.91        1.39        2.56
## z1=0,w2y4 3.64        1.93        7.35
## z1=0,w2y5 1.54        0.94        2.42
## z1=0,w2y6 0.63        0.45        0.86
## z2=0,w1y1 1.30        0.74        2.29
## z2=0,w1y2 0.89        0.64        1.20
## z2=0,w1y3 0.52        0.39        0.72
## z2=0,w1y4 0.27        0.13        0.51
## z2=0,w1y5 0.65        0.41        1.05
## z2=0,w1y6 1.59        1.16        2.21
## z2=0,w2y1 0.77        0.43        1.34
## z2=0,w2y2 1.12        0.83        1.55
## z2=0,w2y3 1.91        1.39        2.56
## z2=0,w2y4 3.64        1.93        7.35
## z2=0,w2y5 1.54        0.94        2.42
## z2=0,w2y6 0.63        0.45        0.86
    #
    predict(mod.fit, pair = "ZY")
## Bootstrap Progress: 
##   |======================================================================| 100%
## Observed odds ratios with 95% asymptotic confidence intervals 
##             OR lower.bound upper.bound
## w1=1,y1z1 0.46        0.25        0.85
## w1=1,y1z2 2.17        1.18        4.00
## w1=1,y2z1 0.84        0.58        1.22
## w1=1,y2z2 1.18        0.82        1.71
## w1=1,y3z1 1.32        0.90        1.94
## w1=1,y3z2 0.76        0.52        1.11
## w1=1,y4z1 0.44        0.15        1.29
## w1=1,y4z2 2.27        0.78        6.62
## w1=1,y5z1 0.77        0.41        1.45
## w1=1,y5z2 1.30        0.69        2.43
## w1=1,y6z1 0.67        0.47        0.96
## w1=1,y6z2 1.49        1.05        2.13
## w2=1,y1z1 1.19        0.49        2.94
## w2=1,y1z2 0.84        0.34        2.06
## w2=1,y2z1 1.12        0.67        1.89
## w2=1,y2z2 0.89        0.53        1.49
## w2=1,y3z1 0.99        0.61        1.61
## w2=1,y3z2 1.01        0.62        1.65
## w2=1,y4z1 0.63        0.29        1.39
## w2=1,y4z2 1.59        0.72        3.51
## w2=1,y5z1 0.93        0.45        1.93
## w2=1,y5z2 1.08        0.52        2.23
## w2=1,y6z1 0.64        0.39        1.06
## w2=1,y6z2 1.55        0.94        2.55
## w1=0,y1z1 1.19        0.49        2.94
## w1=0,y1z2 0.84        0.34        2.06
## w1=0,y2z1 1.12        0.67        1.89
## w1=0,y2z2 0.89        0.53        1.49
## w1=0,y3z1 0.99        0.61        1.61
## w1=0,y3z2 1.01        0.62        1.65
## w1=0,y4z1 0.63        0.29        1.39
## w1=0,y4z2 1.59        0.72        3.51
## w1=0,y5z1 0.93        0.45        1.93
## w1=0,y5z2 1.08        0.52        2.23
## w1=0,y6z1 0.64        0.39        1.06
## w1=0,y6z2 1.55        0.94        2.55
## w2=0,y1z1 0.46        0.25        0.85
## w2=0,y1z2 2.17        1.18        4.00
## w2=0,y2z1 0.84        0.58        1.22
## w2=0,y2z2 1.18        0.82        1.71
## w2=0,y3z1 1.32        0.90        1.94
## w2=0,y3z2 0.76        0.52        1.11
## w2=0,y4z1 0.44        0.15        1.29
## w2=0,y4z2 2.27        0.78        6.62
## w2=0,y5z1 0.77        0.41        1.45
## w2=0,y5z2 1.30        0.69        2.43
## w2=0,y6z1 0.67        0.47        0.96
## w2=0,y6z2 1.49        1.05        2.13
## 
## Model-predicted odds ratios with 95% asymptotic confidence intervals 
##             OR lower.bound upper.bound
## w1=1,y1z1 0.62        0.38        1.01
## w1=1,y1z2 1.62        0.99        2.66
## w1=1,y2z1 0.93        0.69        1.26
## w1=1,y2z2 1.07        0.80        1.45
## w1=1,y3z1 1.20        0.89        1.62
## w1=1,y3z2 0.83        0.62        1.12
## w1=1,y4z1 0.58        0.31        1.09
## w1=1,y4z2 1.71        0.92        3.20
## w1=1,y5z1 0.85        0.53        1.36
## w1=1,y5z2 1.18        0.74        1.90
## w1=1,y6z1 0.66        0.49        0.87
## w1=1,y6z2 1.52        1.14        2.03
## w2=1,y1z1 0.62        0.38        1.01
## w2=1,y1z2 1.62        0.99        2.66
## w2=1,y2z1 0.93        0.69        1.26
## w2=1,y2z2 1.07        0.80        1.45
## w2=1,y3z1 1.20        0.89        1.62
## w2=1,y3z2 0.83        0.62        1.12
## w2=1,y4z1 0.58        0.31        1.09
## w2=1,y4z2 1.71        0.92        3.20
## w2=1,y5z1 0.85        0.53        1.36
## w2=1,y5z2 1.18        0.74        1.90
## w2=1,y6z1 0.66        0.49        0.87
## w2=1,y6z2 1.52        1.14        2.03
## w1=0,y1z1 0.62        0.38        1.01
## w1=0,y1z2 1.62        0.99        2.66
## w1=0,y2z1 0.93        0.69        1.26
## w1=0,y2z2 1.07        0.80        1.45
## w1=0,y3z1 1.20        0.89        1.62
## w1=0,y3z2 0.83        0.62        1.12
## w1=0,y4z1 0.58        0.31        1.09
## w1=0,y4z2 1.71        0.92        3.20
## w1=0,y5z1 0.85        0.53        1.36
## w1=0,y5z2 1.18        0.74        1.90
## w1=0,y6z1 0.66        0.49        0.87
## w1=0,y6z2 1.52        1.14        2.03
## w2=0,y1z1 0.62        0.38        1.01
## w2=0,y1z2 1.62        0.99        2.66
## w2=0,y2z1 0.93        0.69        1.26
## w2=0,y2z2 1.07        0.80        1.45
## w2=0,y3z1 1.20        0.89        1.62
## w2=0,y3z2 0.83        0.62        1.12
## w2=0,y4z1 0.58        0.31        1.09
## w2=0,y4z2 1.71        0.92        3.20
## w2=0,y5z1 0.85        0.53        1.36
## w2=0,y5z2 1.18        0.74        1.90
## w2=0,y6z1 0.66        0.49        0.87
## w2=0,y6z2 1.52        1.14        2.03
## 
## Bootstrap Results: 
## Final results based on 999 resamples 
## Model-predicted odds ratios with 95% bootstrap BCa confidence intervals 
##             OR lower.bound upper.bound
## w1=1,y1z1 0.62        0.37        1.01
## w1=1,y1z2 1.62        0.99        2.67
## w1=1,y2z1 0.93        0.69        1.24
## w1=1,y2z2 1.07        0.81        1.44
## w1=1,y3z1 1.20        0.89        1.62
## w1=1,y3z2 0.83        0.62        1.12
## w1=1,y4z1 0.58        0.31        1.13
## w1=1,y4z2 1.71        0.90        3.30
## w1=1,y5z1 0.85        0.51        1.30
## w1=1,y5z2 1.18        0.77        1.96
## w1=1,y6z1 0.66        0.49        0.88
## w1=1,y6z2 1.52        1.15        2.04
## w2=1,y1z1 0.62        0.37        1.01
## w2=1,y1z2 1.62        0.99        2.67
## w2=1,y2z1 0.93        0.69        1.24
## w2=1,y2z2 1.07        0.81        1.44
## w2=1,y3z1 1.20        0.89        1.62
## w2=1,y3z2 0.83        0.62        1.12
## w2=1,y4z1 0.58        0.31        1.13
## w2=1,y4z2 1.71        0.90        3.30
## w2=1,y5z1 0.85        0.51        1.30
## w2=1,y5z2 1.18        0.77        1.96
## w2=1,y6z1 0.66        0.49        0.88
## w2=1,y6z2 1.52        1.15        2.04
## w1=0,y1z1 0.62        0.37        1.01
## w1=0,y1z2 1.62        0.99        2.67
## w1=0,y2z1 0.93        0.69        1.24
## w1=0,y2z2 1.07        0.81        1.44
## w1=0,y3z1 1.20        0.89        1.62
## w1=0,y3z2 0.83        0.62        1.12
## w1=0,y4z1 0.58        0.31        1.13
## w1=0,y4z2 1.71        0.90        3.30
## w1=0,y5z1 0.85        0.51        1.30
## w1=0,y5z2 1.18        0.77        1.96
## w1=0,y6z1 0.66        0.49        0.88
## w1=0,y6z2 1.52        1.15        2.04
## w2=0,y1z1 0.62        0.37        1.01
## w2=0,y1z2 1.62        0.99        2.67
## w2=0,y2z1 0.93        0.69        1.24
## w2=0,y2z2 1.07        0.81        1.44
## w2=0,y3z1 1.20        0.89        1.62
## w2=0,y3z2 0.83        0.62        1.12
## w2=0,y4z1 0.58        0.31        1.13
## w2=0,y4z2 1.71        0.90        3.30
## w2=0,y5z1 0.85        0.51        1.30
## w2=0,y5z2 1.18        0.77        1.96
## w2=0,y6z1 0.66        0.49        0.88
## w2=0,y6z2 1.52        1.15        2.04
