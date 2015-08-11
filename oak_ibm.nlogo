extensions [profiler matrix]
globals [
  xcutoff ycutoff
  
  surv-params growth-params
  
  basal-area basal-area-ft basal-area-ovs   
  qdbh qdbh-in qdbh-ovs  
  dens dens-ac dens-ovs
  
  ba-oak ba-oak-ovs
  qdbh-oak qdbh-oak-ovs
  dens-oak dens-oak-ovs
  
  ba-map ba-map-ovs
  qdbh-map qdbh-map-ovs
  dens-map dens-map-ovs
  
  ba-pop ba-pop-ovs
  qdbh-pop qdbh-pop-ovs
  dens-pop dens-pop-ovs
 
  prop-oak prop-tol prop-intol
  
  sitequal-boak sitequal-woak sitequal-maple sitequal-poplar
  
  harvest-year shelter-phase
  
  mast-mean-bo mast-mean-wo
  core-acorn-params-bo buffer-acorn-params-bo
  core-acorn-params-wo buffer-acorn-params-wo
  disp-params-sq
  
  wo-mast-list bo-mast-list mast-year-index bo-weev-list wo-weev-list
  acorn-count total-acorns total-seedlings new-seedlings pct-germ ;Oak regen reporters 
  regen-dens regen-stump-dens
  seedlings-class1 seedlings-class2 seedlings-class3 seedlings-class123 seedlings-class4 acorns-pertree
  ]

turtles-own [in-core]

breed [oaks oak]
oaks-own [species light age dbh height canopy-radius canopy-density actual-growth ba acorn-mean seedling sprout? fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality density browsed 
  ]

breed [maples maple]
maples-own [light age dbh height canopy-radius canopy-density actual-growth ba seedling sprout? fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality density
  ]

breed [poplars poplar]
poplars-own [light age dbh height canopy-radius canopy-density actual-growth ba seedling sprout? fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality density
  ]

breed [acorns acorn]
acorns-own [species weevil cached germ]

patches-own [stem-dens shade in-layer1 in-layer2]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup
  clear-all
  reset-ticks
  
  set xcutoff (x-core / 2)
  set ycutoff (y-core / 2)
  
  let adjust (x-core + buffer * 2) * (y-core + buffer * 2) / 10000
  
  resize-world (-1 * (xcutoff + buffer)) (xcutoff + buffer) (-1 * (ycutoff + buffer)) (ycutoff + buffer)
  
  calc-site-quality
  
  set bo-mast-list [0.0827 0.0489 2.975 0.8809 0.0757 0.1153 0.0661 0.1415 0.05667]
  set wo-mast-list [0.1355 0.0665 0.1092 2.720 0.0367 0.136 0.1070 0.238 0.0406]
  set bo-weev-list [0.1396 0.2057 0.6250 0.9545 0.3744 0.1310 0.1304 0.3424 0.2389]
  set wo-weev-list [0.0198 0.1919 0.3679 0.8571 0.1786 0.1176 0.1695 0.1429 0.0761]
  
  ifelse dispersal-scenario = "yearly-diff" or dispersal-scenario = "treat-yearly-diff" [
    set mast-year-index 5][set mast-year-index 0]
  
  set disp-params-sq matrix:from-row-list [
    ;dispProb, lam, weibSc, weibSh, 2dtA, 2dtB, DispEatenProb, cacheProb, undispeatenProb
    [0.624  7.814  8.535 1.353  62.676 1.450 0.757 0.750 0.707] ;AllYrAllTrtBO 
    [0.614  7.892  8.676 1.384  62.412 1.438 0.799 0.679 0.941] ;AllYrAllTrtWO
    [0.638  8.002  8.875 1.469  92.823 1.821 0.768 0.685 0.636] ;AllYrContBO
    [0.545  7.806  8.616 1.417  71.906 1.591 0.733 0.758 0.933] ;AllYrContWO
    [0.593  7.492  7.799 1.116  14.172 0.750 0.708 0.821 0.848] ;AllYrSheltBO
    [0.743  6.778  7.439 1.350  35.333 1.250 0.883 0.500 0.966] ;AllYrSheltWO
    [0.407  3.987  4.216 1.141   8.920 1.133 0.609 0.972 0.634] ;y11BO
    [0.407  3.987  4.216 1.141   8.920 1.133 0.609 0.972 0.634] ;y11WO
    [0.910  8.006  8.792 1.415  87.878 1.731 0.620 0.222 0.000] ;y12BO
    [0.839  7.091  7.813 1.378  55.027 1.544 0.644 0.679 0.714] ;y12WO
    [0.661  9.618 10.849 1.767 456.982 4.597 0.744 0.905 0.952] ;y13BO
    [0.401  9.119 10.100 1.487 491.705 5.000 0.905 0.667 0.968] ;y13WO
    [0.848 10.624 11.962 1.888 619.826 5.000 0.976 1.000 1.000] ;y14BO
    [0.506  8.112  8.892 1.360  58.660 1.346 1.000 0.000 1.000] ;y14WO
    [0.466  4.409  4.810 1.265  19.285 1.561 0.794 1.000 0.487] ;y11ContBO
    [0.291  2.637  2.559 0.948   1.177 0.750 0.130 0.950 0.839] ;y11SheltBO
    [0.466  4.409  4.810 1.265  19.285 1.561 0.794 1.000 0.487] ;y11ContWO
    [0.291  2.637  2.559 0.948   1.177 0.750 0.130 0.950 0.839] ;y11SheltWO
    [0.873  8.020  8.898 1.567 185.572 2.947 0.646 0.118 0.000] ;y12ContBO
    [1.000  8.274  8.866 1.239  24.993 0.836 0.583 0.500 0.000] ;y12SheltBO
    [0.834  8.274  9.140 1.422  82.102 1.610 0.534 0.705 0.615] ;y12ContWO
    [0.848  5.086  5.693 1.535  52.282 2.319 0.798 0.556 0.875] ;y12SheltWO
    [0.590  9.354 10.568 1.932 470.712 5.000 0.661 0.950 0.976] ;y13ContBO
    [0.958 10.335 11.670 1.817 600.180 5.000 1.000 0.000 0.000] ;y13SheltBO
    [0.336  9.328 10.399 1.558 509.884 5.000 0.900 1.000 0.962] ;y13ContWO
    [0.615  9.479 10.490 1.565 530.731 5.000 0.917 0.000 1.000] ;y13SheltWO
    [0.906 10.914 12.336 2.315 625.842 5.000 0.966 1.000 1.000] ;y14ContBO
    [0.743  9.315 10.153 1.341  85.854 1.396 1.000 0.000 1.000] ;y14SheltBO
    [0.407  7.197  8.043 1.567 113.597 2.437 1.000 0.000 1.000] ;y14ContWO
    [0.678  8.844  9.613 1.307  41.316 1.011 1.000 0.000 1.000] ;y14SheltWO
    [0.619  7.699  8.445 1.367  55.407 1.383 0.769 0.704 0.838] ;AllyrAllTrt
    
    ]
   
  ifelse HEE-mean = TRUE [
    init-stand adjust TRUE 89 11 9 95 499 163] [ ;Initial stand values based on Saunders and Arseneault 2013
    init-stand adjust FALSE mature-oak mature-maple mature-poplar sapling-oak sapling-maple sapling-poplar] ;User defined
  
  ask patches [color-patches]
  calc-global-vars
  set harvest-year burnin
  set shelter-phase 1
  setup-plots
 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
    
  calc-light
  
  set-scenario
  set acorn-count 0
  
  ask turtles with [seedling = FALSE] [
    grow
    check-survival
    if breed = oaks and dbh >= 0.20 and seedlings != "none" [produce-acorns] ;based on Downs and McQuilkin 1944
  ]
  
  ask acorns [
   disperse-mast
   germinate-mast 
  ]
  
  if seedlings = "simple" OR seedlings = "hee" [
    ask oaks with [seedling = TRUE] [
      grow-seedling
      check-seedling-survival
    ]
  ]
  
  ask patches [
    regenerate
    color-patches   
  ]
  
  conduct-harvest
  
  calc-global-vars
  
  tick
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                    Basic JABOWA Procedures                      ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to allometerize [dbh-input]
  ;Based on JABOWA, Botkin 1993
  ;b2 = 2*(Hmax - 137) / Dmax and b3 = (Hmax - 137) / Dmax ^2
  set height (137 + b2 * (dbh-input * 100) - b3 * ((dbh-input * 100) ^ 2)) / 100
  
  ;Based on Kenefic and Nyland 1999 and others
  ;range reported is 0.376 - 0.393; going with .5 for now 
  set canopy-radius max list 1 (0.385 * height / 2)
  
  ;leaf weight based on dbh with C species-specific
  ;Exponent can be anywhere from 1.5 to 3 (Botkin 1993)
  ;let SLA (C * (dbh-input * 100) ^ (2.4))
  let SLA (C * (dbh-input * 100) ^ (2))
  
  ;Beer-Lambert law where light above canopy = 1
  let PHI 1
  ;let k 0.0000756 ;Based on QuabbinPlot206, seems far too small
  ;let k 1 / 6000 ;Based on Botkin 1993, allows too much light under big trees
  ;let k 1 / 3000 ;Happy medium
  let k light-extinct
  set canopy-density min list 0.98 (1 - (PHI * exp(-1 * k * SLA)))
  
  set ba (pi * (dbh / 2) ^ 2) ;Basal area 
end


to-report max-growth-increment [dbh-input height-input] ;Based on JABOWA, Botkin 1993
  let num (G * dbh-input * ((1 - (dbh-input * (137 + (b2 * dbh-input) - (b3 * dbh-input ^ 2))) / (Dmax * Hmax))))
  let denom (274 + (3 * b2 * dbh-input) - (4 * b3 * dbh-input ^ 2))
  report (num / denom)  
end


to-report light-growth-index [light-input tol-input]  
  ;Based on Botkin 1993 and Bonan 1990
  if tol-input = "low" [    
    report (2.24 * (1 - exp(-1.136 * (light-input - 0.08))))       
  ] 
  if tol-input = "intermediate" [
    report (1.371 * (1 - exp(-2.227 * (light-input - 0.05))))
  ]  
  if tol-input = "high" [
    report (1 - exp(-4.64 * (light-input - 0.05)))   
  ]  
end


to-report degree-days-index [degdays-input degd-min-input degd-max-input]  
  let tdegd (4 * (degdays-input - degd-min-input) * (degd-max-input - degdays-input) / ((degd-max-input - degd-min-input) ^ 2)) 
  report max list 0 tdegd    
end


to-report saturation-index [wt-input wt-dist-min-input]  
  let wefi (1 - (wt-dist-min-input / wt-input))  
  report max list 0 wefi  
end


to-report nitrogen-index [N-input N-tol-input]  
  if N-tol-input = "intolerant" [
    let a1 2.99 let a2 0.00175 let a3 207.43 let a4 -5 let a5 2.9 let a6 3.671
    let lambdaN (a1 * (1 - 10 ^ (-1 * a2 * (N-input + a3))))
    report (a4 + a5 * lambdaN) / a6
    ]
  if N-tol-input = "intermediate" [
    let a1 2.94 let a2 0.00234 let a3 117.52 let a4 -1.2 let a5 1.3 let a6 2.622
    let lambdaN (a1 * (1 - 10 ^ (-1 * a2 * (N-input + a3))))
    report (a4 + a5 * lambdaN) / a6
    ]
  if N-tol-input = "tolerant" [
    let a1 2.79 let a2 0.00179 let a3 219.77 let a4 -0.6 let a5 1.0 let a6 2.190
    let lambdaN (a1 * (1 - 10 ^ (-1 * a2 * (N-input + a3))))
    report (a4 + a5 * lambdaN) / a6
    ]  
end


to-report wilt-index [wilt-input wilt-max]
  ;wilt is difference between potential and actual evapotranspiration divided by potential evapotranspiration
  let wifi (1 - (wilt-input / wilt-max) ^ 2)
  report max list 0 wifi
end


to grow

  let max-growth (max-growth-increment (dbh * 100) (height * 100))
  
  set fAL light-growth-index light light-tol  
  let sitequal 1
  ifelse breed = oaks [ifelse species = "WO" [set sitequal sitequal-woak] [set sitequal sitequal-boak]] 
  [ifelse breed = maples [set sitequal sitequal-maple] 
    [set sitequal sitequal-poplar]]
  
  set actual-growth (max-growth * fAL * sitequal / max list 1 density) ;Penalize growth based on density of larger trees
  set dbh (dbh * 100 + actual-growth) / 100 
  allometerize dbh
  
  if dbh >= 0.1 and shape = "square" [
    if breed = oaks [set size 2 set shape "tree"]
    if breed = maples [set size 2 set shape "tree"]
    if breed = poplars [set size 2 set shape "tree"]
  ]
  
end


to check-survival
  
  ifelse dbh >= 0.05 [ 
    if random-float 1 < intrinsic-mortality [create-sprout] 
    if actual-growth < min-increment and random-float 1 < growth-mortality [create-sprout]
  ] [
    if random-float 1 < intrinsic-mortality [die] 
    if actual-growth < min-increment and random-float 1 < growth-mortality [die]
  ]  
  set age age + 1
  
end


to create-sprout
  ifelse sprouting = FALSE [die][
  let prob 0
  if dbh < 0.8 and dbh > 0.05 [  
    ;if breed = oaks [set prob (1 + exp(-1 * (5.9991 - 0.2413 * (dbh * 39.3701) - 0.0475 * age))) ^ (-1)] ;From Dey 2002    
    if breed = oaks [ ;From Weigel and Peng 2002
      if species = "WO" [
        set prob (1 + exp(-1 * (-53.6225 - 1.7003 * ln(dbh * 100) - 0.00534 * age * ln(dbh * 100) + 25.7155 * ln(22) - 0.2913 * 22 * ln(22)))) ^ (-1)]
      if species = "BO" [
        set prob (1 + exp(-1 * (-8.1468 - 0.00055 * age * (dbh * 100) + 3.1678 * ln(22)))) ^ (-1)]
    ]   
    if breed = maples [set prob -0.314 * ln(dbh) + 0.0877] ;Based on Powell 1983
    if breed = poplars [set prob 1.1832 - 1.3638 * dbh] ;based on Wendel 1975, True 1953, Beck & Della-Bianca 1981
    if prob > 1 [set prob 1]
  ]
  ifelse random-float 1 < prob [
    set sprout? TRUE
    set dbh 0.01
    init-params
    set size 2
  ]
  [die] 
  ] 
end


to regenerate
  if count turtles-here with [seedling = FALSE] > 0 [stop]  
  let AL light-growth-index ((1 - shade)) "high"
  let Al-pop light-growth-index ((1 - shade)) "low"
  let max-maple-saplings 3 let max-poplar-saplings 10
  
  if random-float 1 < (0.01 * max-maple-saplings * AL * sitequal-maple)[
    sprout-maples 1 [
      check-in-core
      set dbh random-float 0.00468   
      init-params
    ]]
  
  if shade <= 0.01 and random-float 1 < (0.01 * max-poplar-saplings * AL-pop * sitequal-poplar)[
    sprout-poplars 1 [
      check-in-core  
      set dbh random-float 0.0039  
      init-params
    ]]
  
  if seedlings = "none" [
    let Al-oak light-growth-index ((1 - shade) / max list 1 stem-dens ) "intermediate"
    let max-oak-saplings 10
    let sitequal-oak 1
    ifelse random-float 1 < 0.5 [set sitequal-oak sitequal-woak][set sitequal-oak sitequal-boak]
    
    ;arbitrary value here
    if shade < 0.50 and random-float 1 < (0.01 * max-oak-saplings * AL-oak * sitequal-oak)[
    sprout-oaks 1 [
      check-in-core 
      set dbh random-float 0.0041 
      ifelse random-float 1 > 0.5 [set species "WO"][set species "BO"]
      init-params
    ]]
    
  ]
        
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                Early Oak Lifecycle Procedures                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-scenario
  
  let random-index random 8 + 1
  
  ;Acorn production
  if mast-scenario = "fixedaverage" [
    set mast-mean-wo 0.08518
    set mast-mean-bo 0.09033
  ]
  if mast-scenario = "fixedbad" [
    set mast-mean-wo 0.11657
    set mast-mean-bo 0.26672
  ]
  if mast-scenario = "fixedgood" [
    set mast-mean-wo 0.04475
    set mast-mean-bo 0.06030
  ]
  if mast-scenario = "random" [
    set mast-mean-wo item random-index wo-mast-list
    set mast-mean-bo item random-index bo-mast-list
  ]
  if mast-scenario = "hee" [
    set mast-mean-wo item mast-year-index wo-mast-list
    set mast-mean-bo item mast-year-index bo-mast-list
  ]
  
  if mast-scenario = "priorgood" [
    ifelse ticks = (harvest-year - 1) OR ticks = (harvest-year - 2) [
      set mast-mean-wo 0.04475
      set mast-mean-bo 0.06030
      ][
      set mast-mean-wo one-of wo-mast-list
      set mast-mean-bo one-of bo-mast-list
      ]    
  ]
  if mast-scenario = "priorbad" [
    ifelse ticks = (harvest-year - 1) OR ticks = (harvest-year - 2) [
      set mast-mean-wo 0.11567
      set mast-mean-bo 0.26672
      ][
      set mast-mean-wo one-of wo-mast-list
      set mast-mean-bo one-of bo-mast-list
      ]    
  ]
  
  ;Seedling survival and growth
  if seed-scenario = "fixedaverage" [
    ; intercept species canopy age
    set surv-params (list -0.6 0.101 0.366 0.576)
    ; intercept seedSD browse canopy species obsSD
    set growth-params (list 1.24 0.248 -0.94 -0.488 0.166 1.188)
  ]
  
  if seed-scenario = "randomdrought" [
    
    ;drought conditions
    ifelse random-float 1 < drought-prob [
      set surv-params (list -0.531 0.197 0.528 0.44)
      set growth-params (list 0.911 0.157 -0.871 -0.238 0.085 1.016)
      ]
    ;non-drought
    [
      set surv-params (list -0.531 0.197 0.528 0.44)
      set growth-params (list 2.091 0.199 -0.976 -1.733 0.468 1.468)
    ]    
  ]
  
  ;Browsing
  if browse-scenario = "fixedaverage" [set prob-browsed 0.1058] 
  if browse-scenario = "hee" [set prob-browsed one-of (list 0.091 0.0976 0.1793 0.0610)]  
  if browse-scenario = "custom" [ ]
  
  ;Weevils
  if weevil-scenario = "fixedaverage" [set bo-weevil-prob 0.3491 set wo-weevil-prob 0.2357]
  if weevil-scenario = "random" [set bo-weevil-prob one-of bo-weev-list set wo-weevil-prob one-of wo-weev-list]  
  if weevil-scenario = "random-match" [ 
    set bo-weevil-prob item random-index bo-weev-list
    set wo-weevil-prob item random-index wo-weev-list   
    ]  
  if weevil-scenario = "hee" [
    set bo-weevil-prob item mast-year-index bo-weev-list
    set wo-weevil-prob item mast-year-index wo-weev-list
    ]
  if weevil-scenario = "custom" [ ]
    
  if dispersal-scenario = "fixedaverage" [
    ;old defaults  
    ;set core-acorn-params [0.41 5.185 0.704 0.288 0.538]
    ;set buffer-acorn-params [0.41 5.185 0.704 0.288 0.538]   
    set core-acorn-params-bo matrix:get-row disp-params-sq 30
    set buffer-acorn-params-bo matrix:get-row disp-params-sq 30
    set core-acorn-params-wo matrix:get-row disp-params-sq 30
    set buffer-acorn-params-wo matrix:get-row disp-params-sq 30
    ]
  
  if dispersal-scenario = "custom" [
    set dispersal-distrib "exponential"
    set core-acorn-params-bo (list disperse-prob disperse-dist 0 0 0 0 disperse-eaten-prob cache-prob undisp-eaten-prob)
    set buffer-acorn-params-bo (list disperse-prob disperse-dist 0 0 0 0 disperse-eaten-prob cache-prob undisp-eaten-prob)
    set core-acorn-params-wo (list disperse-prob disperse-dist 0 0 0 0 disperse-eaten-prob cache-prob undisp-eaten-prob)
    set buffer-acorn-params-wo (list disperse-prob disperse-dist 0 0 0 0 disperse-eaten-prob cache-prob undisp-eaten-prob)
    ]
  
  if dispersal-scenario = "treat-diff" [
    set core-acorn-params-bo matrix:get-row disp-params-sq 4
    set buffer-acorn-params-bo matrix:get-row disp-params-sq 2
    set core-acorn-params-wo matrix:get-row disp-params-sq 5
    set buffer-acorn-params-wo matrix:get-row disp-params-sq 3     
  ]
  
  if dispersal-scenario = "yearly-diff" [
    let translate-index-bo (list 10 10 10 10 10 10 6 8 10 12)
    let translate-index-wo (list 10 10 10 10 10 10 7 9 11 13)
    set core-acorn-params-bo matrix:get-row disp-params-sq (item (mast-year-index + 1) translate-index-bo)
    set buffer-acorn-params-bo matrix:get-row disp-params-sq (item (mast-year-index + 1) translate-index-bo)
    set core-acorn-params-wo matrix:get-row disp-params-sq (item (mast-year-index + 1) translate-index-wo)
    set buffer-acorn-params-wo matrix:get-row disp-params-sq (item (mast-year-index + 1) translate-index-wo)   
    ]
  if dispersal-scenario = "treat-yearly-diff" [ ]
  
  set mast-year-index (mast-year-index + 1)
  if mast-year-index = 9 [
      ifelse dispersal-scenario = "yearly-diff" or dispersal-scenario = "treat-yearly-diff"[
        set mast-year-index 5] [set mast-year-index 0]]
     
end

to produce-acorns 
  
  let acorns-produced 0
  let weevil-probability 0
  ifelse species = "WO" [
    set acorns-produced (pi * canopy-radius ^ 2 * random-exponential (1 / mast-mean-wo)) ;per meter squared
    set weevil-probability wo-weevil-prob]
  [ 
    set acorns-produced (pi * canopy-radius ^ 2 * random-exponential (1 / mast-mean-bo))
    set weevil-probability bo-weevil-prob]
  if in-core = TRUE [set acorn-count (acorn-count + acorns-produced)]  
  let tree-radius canopy-radius
  let temp species
  let coretemp in-core
  
  hatch-acorns acorns-produced [
  ;;drop produced acorns under canopy randomly
    set species temp
    set in-core coretemp 
    set hidden? FALSE
    right random 360
    forward (random-float (tree-radius - 0.5)) + 0.5
    
    ifelse random-float 1 < weevil-probability [set weevil TRUE] [set weevil FALSE] ;;check to see if weeviled
  ]
  
end


to disperse-mast

  ;Set up parameter sets depending on species
  let core-acorn-params core-acorn-params-bo
  let buffer-acorn-params buffer-acorn-params-bo
  if species = "WO" [
    set core-acorn-params core-acorn-params-wo 
    set buffer-acorn-params buffer-acorn-params-wo
  ]
  
  let acorn-params buffer-acorn-params
  if in-core = TRUE and ticks > harvest-year [set acorn-params core-acorn-params]
  
  ;;move mast via "dispersers"
  ;;removal probability - HEE dispersal data for WO
  ifelse random-float 1 < (item 0 acorn-params) and weevil = FALSE [
    
    ;;new approach to dispersal to avoid openings
    let startx xcor let starty ycor    
    loop [
      right random 360
      ;;based on HEE data
      ifelse dispersal-distrib = "exponential" [forward random-exponential (item 1 acorn-params)]
      [forward random-weibull (item 3 acorn-params) (item 2 acorn-params)]
      ifelse [shade] of patch-here > 0.2 [stop]
      [set xcor startx set ycor starty]      
      ]
    
    ;;check to see if still in core area after being dispersed
    check-in-core
    ifelse in-core = TRUE and ticks > harvest-year [set acorn-params core-acorn-params]
    [set acorn-params buffer-acorn-params]
    
    
    ;;probability of being eaten
    ifelse random-float 1 > (item 6 acorn-params) [
      ;;probability of being cached
      ifelse random-float 1 < (item 7 acorn-params) [set cached TRUE] [set cached FALSE]]
    [die]]
  ;;check if eaten
  [if random-float 1 < (item 8 acorn-params) [die]] 
end


to germinate-mast
  let temp species
  ifelse cached = TRUE [
    ;Based on Haas and Heske 2005 (0.77 for buried)
    set germ germ-prob]
  ;;Based on:
  ;;Lombardo & McCarthy 2009 (0.26 germ for weeviled) 
  ;;and Haas and Heske 2005 (0.09 for surfaced)
   [ifelse weevil = TRUE [set germ 0.09 * 0.26]  
    [set germ 0.09]]    
  if random-float 1 < germ [
    hatch-oaks 1 [
      set species temp
      set age 0
      set size 1
      set seedling TRUE   
      set hidden? TRUE
      set height 0.092 ;based on HEE seedlings
    ]]
    die
end


to grow-seedling
  set light (1 - [shade] of patch-here)
  ifelse random-float 1 < prob-browsed [set browsed 1][set browsed 0]
  
  ifelse seedlings = "hee" [
    ;ifelse random-float 1 < 0.0699 and light > 0.8 [
    ;  set actual-growth (min list 150 random-exponential 52.18) / 100
    ;][
    ;let sp-ef 0
    ;if species = "WO" [set sp-ef 1.73]
    ;set actual-growth max list 0 (4.485 + random-normal 0 1.934 - 3.014 * (1 - light) 
    ;  + sp-ef - 4.892 * browsed + random-normal 0 9.151) / 100
    ;]
    
    let sp-ef 0
    if species = "WO" [set sp-ef 1]
    let growthpred ((item 0 growth-params) + (random-normal 0 item 1 growth-params) 
    + (item 2 growth-params) * browsed + (item 3 growth-params) * (1 - light)     
    + (item 4 growth-params) * sp-ef + (random-normal 0 item 5 growth-params))
    
    set actual-growth (inv-neglog growthpred) / 100
    
    if actual-growth < 0 [set actual-growth 0]
   
  ][
  
  let max-growth seedling-growth  ;growth is height-based 
  set fAL light-growth-index light "intermediate"
  set actual-growth (max-growth * fAL)
  
  ]
  
  set height (height + actual-growth)

  
  if height > 1.37 [ ;When seedling reaches dbh 0.01, height = 2.039
    
    set dbh max list 0 ((-1.88 + 0.01372 * (height * 100)) / 100)
    init-params ;Convert to sapling
  ]
end


to check-seedling-survival ;Based on HEE data
  
  if age = 0 [if random-float 1 > 0.567 [die]]
  
  let sp-ef 0
  if species = "WO" [set sp-ef 1]
 
  let survpred ((item 0 surv-params) + (item 1 surv-params) * sp-ef + (item 2 surv-params) 
    * (1 - light) + (item 3 surv-params) * (min list 5 age))  
    
  let psurv (1 + exp(-1 * survpred)) ^ (-1) 
  if random-float 1 > psurv [die]
  
  set age age + 1
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                Light Calculation Procedures                     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to calc-light 
 ask patches [set pcolor brown set stem-dens 0 set shade 0] 
 foreach sort-by [[height] of ?1 > [height] of ?2] turtles with [seedling = FALSE] [
    ask ? [
      let start-ycor ycor
      let temp canopy-density
      set light (1 - mean [shade] of patches in-radius max list 3 canopy-radius)
      set density [stem-dens] of patch-here ;Stem density will penalize growth
      ask patches in-radius density-dep [set stem-dens stem-dens + 1] ;Radius calibrated to prevent giant overshot in basal area over time
      
      ;draw initial canopy
      ;Shape of shade projection based on ShadeMotion simulation for 39 degrees latitude
      ifelse canopy-radius < 2 [
        ;Calculate overlapping shade instead of summing up leaf-weight; mathematically equivalent and fewer calculations to make.
        ask patches in-radius canopy-radius [set shade (shade + (1 - shade) * temp) ]]
      [
        set ycor (ycor + canopy-radius / 2)
        ask patches in-radius canopy-radius [
        ;Interior circle  
        set in-layer1 TRUE
        set shade (shade + (1 - shade) * temp)]
      
      ;smaller ellipse (minus the initial canopy)
      set ycor (ycor + canopy-radius / 2.5)
      let test1 in-ellipse (canopy-radius * 2) (canopy-radius * 1.1) 1
      ask test1 [ 
        set in-layer2 TRUE
        set shade (shade + (1 - shade) * temp * 0.5)]
      
      ;larger ellipse (minus the smaller ellipse)
      set ycor (ycor + canopy-radius / 4.5)
      let test2 in-ellipse (canopy-radius * 3) (canopy-radius * 1.5) 2
      ask test2 [ 
        set shade (shade + (1 - shade) * temp * 0.25)] 
       
      ;reset layering 
      let test3 in-ellipse (canopy-radius * 3) (canopy-radius * 1.5) 3
      ask test3 [set in-layer1 FALSE set in-layer2 FALSE]
      ] 
      
      ;move tree back to starting location
      set ycor start-ycor

      
 ]]
end

;Modified from Wilensky, U. (2003) Netlogo Fur Model
to-report in-ellipse [x-radius y-radius layer]  ;; patch procedure
  
  if layer = 1 [
    report other patches in-radius (max list x-radius y-radius)
           with [1.0 >= ((xdistance myself ^ 2) / (x-radius ^ 2)) +
                        ((ydistance myself ^ 2) / (y-radius ^ 2)) and in-layer1 = FALSE]]
  if layer = 2 [
    report other patches in-radius (max list x-radius y-radius)
           with [1.0 >= ((xdistance myself ^ 2) / (x-radius ^ 2)) +
                        ((ydistance myself ^ 2) / (y-radius ^ 2)) 
                        and in-layer2 = FALSE and in-layer1 = FALSE]]
  if layer = 3 [
    report other patches in-radius (max list x-radius y-radius)
           with [1.0 >= ((xdistance myself ^ 2) / (x-radius ^ 2)) +
                        ((ydistance myself ^ 2) / (y-radius ^ 2))]]
end


to-report xdistance [other-patch]  ;; patch procedure
  let xdiff abs (pxcor - [pxcor] of other-patch)
  report min (list xdiff (world-width - xdiff))
end

to-report ydistance [other-patch]  ;; patch procedure
  let ydiff abs (pycor - [pycor] of other-patch)
  report min (list ydiff (world-height - ydiff))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                       Utility Procedures                        ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report random-weibull [Wshape Wscale]
  let R random-float 1
  report Wscale * (- ln(R)) ^ (1 / Wshape)
end

to check-in-core
  ifelse xcor < xcutoff and xcor > (-1 * xcutoff) and ycor < ycutoff and ycor > (-1 * ycutoff)[set in-core TRUE][set in-core FALSE]
end

to-report inv-neglog [x]
  ifelse x <= 1 [report (1 - exp(-1 * x))][report (exp(x) - 1)]
end

to calc-site-quality
  ifelse manual-site-qual = TRUE [
    
    set sitequal-woak sqwoak
    set sitequal-boak sqboak
    set sitequal-maple sqmap
    set sitequal-poplar sqpop
    
  ][
  ;based bon Botkin 1993 and Holm 2012 with some guesses
  ;white oak
  let fT degree-days-index DegDays 1977 5894 
  let fWT saturation-index wt-dist 0.933
  let fN nitrogen-index available-N "intermediate"
  let fWL wilt-index wilt 0.45
  set sitequal-woak fT * fWT * fN * fWL
  ;black oak
  set fT degree-days-index DegDays 2068 5421 
  set fWT saturation-index wt-dist 0.933
  set fN nitrogen-index available-N "tolerant"
  set fWL wilt-index wilt 0.45
  set sitequal-boak fT * fWT * fN * fWL
  ;maple
  set fT degree-days-index DegDays 2000 6300 
  set fWT saturation-index wt-dist 0.567
  set fN nitrogen-index available-N "intermediate"
  set fWL wilt-index wilt 0.35
  set sitequal-maple fT * fWT * fN * fWL
  ;poplar
  set fT degree-days-index DegDays 2171 6363 
  set fWT saturation-index wt-dist 0.544 ;;based on white spruce/red maple (similar moisture tolerance) ??
  set fN nitrogen-index available-N "intolerant"
  set fWL wilt-index wilt 0.245 ;;based on white spruce/red maple (similar moisture tolerance) ??
  set sitequal-poplar fT * fWT * fN * fWL
  ]
end

to init-stand [dens-adjust space-adjust n-oak n-maple n-poplar n-sap-oak n-sap-maple n-sap-poplar]
  
  ;;Create adult trees
  create-oaks dens-adjust * n-oak [ ;dens-adjust adds more trees when there is a buffer
    ifelse random-float 1 > 0.5 [set species "WO"][set species "BO"]
    set dbh (random-normal 45.75 5) / 100
    init-params  
    set age round (Amax * (dbh * 100) / Dmax)   
    ifelse space-adjust = TRUE [
      loop [
        setxy random-xcor random-ycor
        if count oaks in-radius 7 < 2 [stop]]]
    [setxy random-xcor random-ycor]
  ]  
  create-maples dens-adjust * n-maple [
    set dbh (random-normal 40.8 5) / 100
    init-params
    set age round (Amax * (dbh * 100) / Dmax)      
    ifelse space-adjust = TRUE [
      loop [
        setxy random-xcor random-ycor
        if count turtles in-radius 8 < 2 [stop]]]
    [setxy random-xcor random-ycor]
  ] 
  create-poplars dens-adjust * n-poplar [
    set dbh (random-normal 45.07 5) / 100
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    ifelse space-adjust = TRUE [
      loop [
        setxy random-xcor random-ycor
        if count turtles in-radius 8 < 2 [stop]]]
    [setxy random-xcor random-ycor]
  ]

  calc-light
  
  ;;Create saplings
  create-oaks dens-adjust * n-sap-oak [
    set dbh max list 0.015 ((random-normal 14.9 5) / 100)
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    loop [
      setxy random-xcor random-ycor
      if mean [shade] of patches in-radius 2 < 0.6 [stop]]
  ]  
  create-maples dens-adjust * n-sap-maple [
    set dbh max list 0.015 ((random-normal 10.3 5) / 100)
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    setxy random-xcor random-ycor
  ] 
  create-poplars dens-adjust * n-sap-poplar [
    set dbh max list 0.015 ((random-normal 14.9 5) / 100)
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    loop [
      setxy random-xcor random-ycor
      if mean [shade] of patches in-radius 2 < 0.6 [stop]]
  ]
   
  calc-light  
  ask turtles [check-in-core]
  
end

to init-params
  ifelse dbh < 0.1 [set shape "square"] [set size 2 set shape "tree"]
  set hidden? FALSE
  set actual-growth 0.02
  set min-increment 0.01
  set growth-mortality 0.369
  set age 1
  set seedling FALSE
  if breed = oaks [ 
    ifelse species = "WO" [;White oak based on Botkin 1993 and Holm 2012
      set color white
      set Dmax 100 set Hmax 3800 ;based on HEE data
      set Amax 400 set intrinsic-mortality 4.0 / Amax ;Based on 2% reaching max age; common to all species
      set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
      set C 1.75
      set G 104
      set light-tol "intermediate"
      set acorn-mean random-exponential 10 ;;mean oak production / m2 based on HEE histogram
    ][set color black ;Black oak based on Botkin 1993 and Holm 2012
      set Dmax 100 set Hmax 3800 ;based on HEE data
      set Amax 300 set intrinsic-mortality 4.0 / Amax ;Based on 2% reaching max age; common to all species
      set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
      set C 1.75
      set G 122
      set light-tol "intermediate"
      set acorn-mean random-exponential 10 ;;mean oak production / m2 based on HEE histogram
    ]]
  if breed = maples [ ;Sugar maple based bon Botkin 1993 and Holm 2012
    set color sky
    set Dmax 100 set Hmax 3350 ;set Dmax 170
    set Amax 400 set intrinsic-mortality 4.0 / Amax
    set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
    set C 1.57
    set G 118.7
    set light-tol "high"
  ]
  if breed = poplars [ ;Tulip poplar based on Holm 2012 with some guesses
    set color red  
    set Dmax 100 set Hmax 4000 
    set Amax 300 set intrinsic-mortality 4.0 / Amax
    set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)  
    set C 1.75 ;assumed to be similar to oak
    set G 140
    set light-tol "low"
  ]
  allometerize dbh
    
end

to calc-global-vars ;;Calculate global reporter values
    
  let adjust (x-core * y-core) / 10000
  
  set basal-area (sum [ba] of turtles with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set basal-area-ovs (sum [ba] of turtles with [dbh >= 0.3 and in-core = TRUE]) / adjust
  set basal-area-ft basal-area * 4.356
  set ba-oak (sum [ba] of oaks with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set ba-oak-ovs (sum [ba] of oaks with [dbh >= 0.3 and in-core = TRUE]) / adjust
  set ba-map (sum [ba] of maples with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set ba-map-ovs (sum [ba] of maples with [dbh >= 0.3 and in-core = TRUE]) / adjust
  set ba-pop (sum [ba] of poplars with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set ba-pop-ovs (sum [ba] of poplars with [dbh >= 0.3 and in-core = TRUE]) / adjust
  set dens (count turtles with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set dens-ovs (count turtles with [dbh >= 0.3 and in-core = TRUE]) / adjust
  set dens-ac dens * 0.40477
  set dens-oak (count oaks with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set dens-oak-ovs (count oaks with [dbh >= 0.3 and in-core = TRUE]) / adjust
  set dens-map (count maples with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set dens-map-ovs (count maples with [dbh >= 0.3 and in-core = TRUE]) / adjust
  set dens-pop (count poplars with [dbh >= 0.015 and in-core = TRUE]) / adjust
  set dens-pop-ovs (count poplars with [dbh >= 0.3 and in-core = TRUE]) / adjust  
  ifelse basal-area > 0 [
    set qdbh sqrt(basal-area * adjust / (0.0000785 * count turtles with [dbh >= 0.015 and in-core = TRUE]))
    set qdbh-in 2 * (sqrt(mean [ba] of turtles with [dbh >= 0.015 and in-core = TRUE] / pi)) * 39.37 
    set prop-oak (ba-oak / basal-area)
    set prop-tol (ba-map / basal-area)
    set prop-intol (ba-pop / basal-area)
  ][set qdbh 0 set qdbh-in 0 set prop-oak 0 set prop-tol 0 set prop-intol 0]
  set qdbh-ovs sqrt(basal-area-ovs * adjust / (0.0000785 * (max list 1 count turtles with [dbh >= 0.3 and in-core = TRUE])))  
  set qdbh-oak sqrt(ba-oak * adjust / (0.0000785 * (max list 1 count oaks with [dbh >= 0.015 and in-core = TRUE])))
  set qdbh-oak-ovs sqrt(ba-oak-ovs * adjust / (0.0000785 * (max list 1 count oaks with [dbh >= 0.3 and in-core = TRUE])))
  set qdbh-map sqrt(ba-map * adjust / (0.0000785 * (max list 1 count maples with [dbh >= 0.015 and in-core = TRUE])))
  set qdbh-map-ovs sqrt(ba-map-ovs * adjust / (0.0000785 * (max list 1 count maples with [dbh >= 0.3 and in-core = TRUE])))
  set qdbh-pop sqrt(ba-pop * adjust / (0.0000785 * (max list 1 count poplars with [dbh >= 0.015 and in-core = TRUE])))
  set qdbh-pop-ovs sqrt(ba-pop-ovs * adjust / (0.0000785 * (max list 1 count poplars with [dbh >= 0.3 and in-core = TRUE])))
  set total-seedlings (count turtles with [seedling = TRUE and in-core = TRUE]) / adjust
  set new-seedlings (count turtles with [seedling = TRUE and age = 1 and in-core = TRUE]) / adjust
  set seedlings-class1 (count turtles with [seedling = TRUE and height <= 0.3 and in-core = TRUE]) / adjust
  set seedlings-class2 (count turtles with [seedling = TRUE and height > 0.3 and height <= 0.6 and in-core = TRUE]) / adjust
  set seedlings-class3 (count turtles with [seedling = TRUE and height > 0.6 and height <= 1.4 and in-core = TRUE]) / adjust
  set seedlings-class123 (count turtles with [seedling = TRUE and height <= 1.4 and in-core = TRUE]) / adjust
  set seedlings-class4 (count oaks with [height > 1.4 and dbh < 0.015 and in-core = TRUE]) / adjust
  set total-acorns round (acorn-count / adjust)
  ifelse count oaks with [dbh > 0.2 and in-core = TRUE] > 0 [set acorns-pertree (total-acorns / count oaks with [dbh > 0.2 and in-core = TRUE])]
  [set acorns-pertree 0]
  ifelse total-acorns > 0 [set pct-germ (new-seedlings / total-acorns)][set pct-germ 0]
  set regen-dens (count oaks with [height >= 1.37 and height < 5 and in-core = TRUE]) / adjust
  set regen-stump-dens (count oaks with [sprout? = TRUE and height >= 1.37 and height < 5 and in-core = TRUE]) / adjust
  
end

to color-patches
  if shade > 0 [set pcolor lime + 1]
  if shade > 0.1 [set pcolor lime]
  if shade > 0.2 [set pcolor lime - 1]
  if shade > 0.3 [set pcolor green + 2]
  if shade > 0.5 [set pcolor green + 1]
  if shade > 0.7 [set pcolor green]
  if shade > 0.8 [set pcolor green - 1]
  if shade > 0.9 [set pcolor green - 2]
  if shade > 0.95 [set pcolor green - 3]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                    Harvesting Procedures                        ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to conduct-harvest
  
  if harvest-type = "none" [stop]
  
  if (ticks + 1) != harvest-year [stop]
  
  let adjust (x-core * y-core) / 10000
  
  if harvest-type = "clearcut" [
    ask turtles with [dbh > 0.01 and in-core = TRUE] [create-sprout]
    ;set harvest-year harvest-year + 100
  ]
  
  if harvest-type = "singletree" [
    set basal-area (sum [ba] of turtles with [dbh >= 0.01 and in-core = TRUE]) / adjust
    set harvest-year harvest-year + 20 
    if basal-area >= 25 [
      loop [
        let potential one-of turtles with [dbh >= 0.10 and in-core = TRUE]
        ask potential [create-sprout]
        set basal-area (sum [ba] of turtles with [dbh >= 0.01 and in-core = TRUE]) / adjust
        if basal-area <= 25 [stop]
        ]
    ]
  ]
  
  if harvest-type = "shelterwood" [
    ifelse shelter-phase = 1 [
      set shelter-phase 2
      ask turtles with [breed != oaks and dbh <= 0.254 and in-core = TRUE] [die] ;treated with herbicide
      set harvest-year (harvest-year + 7)
    ] 
    [
      ifelse shelter-phase = 2 [
        set shelter-phase 3
        set harvest-year (harvest-year + 8)
        if basal-area >= 16.1 [
          loop [
            ;this code is broken
            ;let potential min-one-of turtles with [breed != oaks and dbh >= 0.10 and xcor < 50 and xcor > -50 and ycor < 50 and ycor > -50] [dbh]
            let potential min-one-of turtles with [age > 20 and in-core = TRUE] [light]
            ask potential [ifelse breed = oaks [create-sprout][die]]
            set basal-area (sum [ba] of turtles with [dbh >= 0.01 and in-core = TRUE]) / adjust
            if basal-area <= 16.1 [stop]
          ]
        ]      
      ] 
      [
        ask turtles with [age > 20 and in-core = TRUE] [create-sprout]
        set shelter-phase 1
        ;set harvest-year (harvest-year + 100)
      ]
       
  ]]
  
end
@#$#@#$#@
GRAPHICS-WINDOW
220
10
935
746
70
70
5.0
1
10
1
1
1
0
1
1
1
-70
70
-70
70
1
1
1
ticks
30.0

BUTTON
34
40
97
73
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
345
759
439
792
mature-oak
mature-oak
0
300
0
1
1
NIL
HORIZONTAL

BUTTON
104
40
167
73
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
1020
655
1195
688
DegDays
DegDays
1980
5500
4840
1
1
NIL
HORIZONTAL

SLIDER
1021
693
1193
726
wt-dist
wt-dist
0.1
10
9.3
0.1
1
m
HORIZONTAL

SLIDER
1021
733
1193
766
available-N
available-N
0
350
325
25
1
kg/ha/yr
HORIZONTAL

MONITOR
33
203
101
248
BA (m2/ha)
basal-area
2
1
11

SLIDER
1102
134
1218
167
bo-weevil-prob
bo-weevil-prob
0
1
0.3491
0.01
1
NIL
HORIZONTAL

SLIDER
1116
306
1228
339
germ-prob
germ-prob
0
1
0.77
0.01
1
NIL
HORIZONTAL

SLIDER
1022
388
1194
421
seedling-growth
seedling-growth
0
1.37
0.6
0.01
1
m
HORIZONTAL

SLIDER
441
759
537
792
sapling-oak
sapling-oak
0
300
0
1
1
NIL
HORIZONTAL

SLIDER
346
795
439
828
mature-maple
mature-maple
0
300
0
1
1
NIL
HORIZONTAL

MONITOR
33
254
100
299
qDBH (cm)
qdbh
2
1
11

MONITOR
108
203
176
248
BA (ft2/ac)
basal-area-ft
2
1
11

MONITOR
111
254
176
299
qDBH (in)
qdbh-in
2
1
11

MONITOR
35
306
99
351
Trees/Ha
dens
2
1
11

MONITOR
112
306
175
351
Trees/Ac
dens-ac
2
1
11

PLOT
8
357
208
507
Basal Area
Years
m2/ha
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range (precision (basal-area - 10) 0) (precision (basal-area + 10) 0)\nplot-pen-up\nplotxy 0 basal-area\nplot-pen-down" ""
PENS
"default" 1.0 0 -16777216 true "" "plot basal-area"

PLOT
9
516
209
666
BA Proportion
Years
NIL
0.0
10.0
0.0
1.0
true
true
"set-plot-y-range (precision 0 0) (precision 1 0)\nplot-pen-up\nplot-pen-down" ""
PENS
"Oak" 1.0 0 -16777216 true "" "plot prop-oak"
"Maple" 1.0 0 -13791810 true "" "plot prop-tol"
"Poplar" 1.0 0 -2674135 true "" "plot prop-intol"

SLIDER
346
831
439
864
mature-poplar
mature-poplar
0
300
0
1
1
NIL
HORIZONTAL

SLIDER
441
795
538
828
sapling-maple
sapling-maple
0
500
0
1
1
NIL
HORIZONTAL

SLIDER
441
831
538
864
sapling-poplar
sapling-poplar
0
500
0
1
1
NIL
HORIZONTAL

SWITCH
222
779
334
812
HEE-mean
HEE-mean
0
1
-1000

TEXTBOX
228
753
378
771
Initial Forest\n
14
0.0
1

SLIDER
1022
771
1194
804
wilt
wilt
0
0.4
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
985
606
1062
651
NIL
sitequal-woak
2
1
11

MONITOR
1065
606
1142
651
NIL
sitequal-maple
2
1
11

MONITOR
1146
606
1226
651
NIL
sitequal-poplar
2
1
11

TEXTBOX
1057
465
1207
483
Site Conditions
14
0.0
1

TEXTBOX
1043
10
1193
28
Oak Regen Parameters
14
0.0
1

TEXTBOX
301
71
451
89
Output
14
0.0
1

TEXTBOX
46
83
196
101
Harvest Controls
14
0.0
1

CHOOSER
33
107
171
152
harvest-type
harvest-type
"none" "clearcut" "shelterwood" "singletree"
2

TEXTBOX
65
15
215
33
Start Model
14
0.0
1

SLIDER
25
703
197
736
light-extinct
light-extinct
0.00016667
0.00033333
2.5E-4
0.00001
1
NIL
HORIZONTAL

SLIDER
26
741
198
774
density-dep
density-dep
0.5
8
3.5
0.5
1
NIL
HORIZONTAL

TEXTBOX
52
682
202
700
Tuning Parameters
14
0.0
1

SWITCH
1031
486
1178
519
manual-site-qual
manual-site-qual
0
1
-1000

SLIDER
1008
523
1100
556
sqwoak
sqwoak
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
1106
522
1198
555
sqboak
sqboak
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
1008
558
1100
591
sqmap
sqmap
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
1105
558
1197
591
sqpop
sqpop
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
246
62
418
95
x-core
x-core
10
250
100
1
1
m
HORIZONTAL

SLIDER
246
102
418
135
y-core
y-core
10
250
100
1
1
m
HORIZONTAL

SLIDER
247
139
419
172
buffer
buffer
0
50
20
1
1
m
HORIZONTAL

SLIDER
19
157
191
190
burnin
burnin
0
100
10
1
1
years
HORIZONTAL

CHOOSER
971
300
1109
345
seedlings
seedlings
"none" "simple" "hee"
2

CHOOSER
958
34
1050
79
mast-scenario
mast-scenario
"random" "hee" "fixedaverage" "fixedgood" "fixedbad" "priorgood" "priorbad"
1

SWITCH
1052
425
1160
458
sprouting
sprouting
0
1
-1000

SLIDER
986
178
1100
211
disperse-prob
disperse-prob
0
1
0.56
0.01
1
NIL
HORIZONTAL

SLIDER
986
219
1098
252
disperse-dist
disperse-dist
0
10
5.185
0.001
1
NIL
HORIZONTAL

SLIDER
1101
219
1217
252
disperse-eaten-prob
disperse-eaten-prob
0
1
0.705
0.001
1
NIL
HORIZONTAL

SLIDER
987
256
1100
289
cache-prob
cache-prob
0
1
0.288
0.001
1
NIL
HORIZONTAL

SLIDER
1104
256
1218
289
undisp-eaten-prob
undisp-eaten-prob
0
1
0.538
0.001
1
NIL
HORIZONTAL

SLIDER
1097
351
1212
384
prob-browsed
prob-browsed
0
1
0.1058
0.01
1
NIL
HORIZONTAL

CHOOSER
1054
33
1146
78
seed-scenario
seed-scenario
"fixedaverage" "randomdrought"
1

SLIDER
986
352
1094
385
drought-prob
drought-prob
0
1
0
0.01
1
NIL
HORIZONTAL

CHOOSER
1148
33
1240
78
browse-scenario
browse-scenario
"fixedaverage" "hee" "custom"
0

SLIDER
981
134
1099
167
wo-weevil-prob
wo-weevil-prob
0
1
0.2357
0.01
1
NIL
HORIZONTAL

CHOOSER
996
84
1095
129
weevil-scenario
weevil-scenario
"fixedaverage" "random" "random-match" "hee" "custom"
0

CHOOSER
1098
84
1194
129
dispersal-scenario
dispersal-scenario
"fixedaverage" "custom" "treat-diff" "yearly-diff"
1

CHOOSER
1105
171
1200
216
dispersal-distrib
dispersal-distrib
"exponential" "weibull"
0

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="densitytest" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="160"/>
    <metric>basal-area</metric>
    <metric>ba-oak</metric>
    <metric>ba-map</metric>
    <metric>ba-pop</metric>
    <enumeratedValueSet variable="mature-poplar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-maple">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weevil-probability">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DegDays">
      <value value="4444"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="available-N">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-oak">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density-dep">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="3.5"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wilt">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-poplar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="light-extinct">
      <value value="2.5E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-maple">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="germ-prob">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HEE-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wt-dist">
      <value value="8.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seedling-growth">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-type">
      <value value="&quot;clearcut&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="harvestexample" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>basal-area</metric>
    <enumeratedValueSet variable="sapling-maple">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seedling-growth">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-maple">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-oak">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HEE-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="available-N">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wilt">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wt-dist">
      <value value="8.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="germ-prob">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-poplar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density-dep">
      <value value="3.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DegDays">
      <value value="4444"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="light-extinct">
      <value value="2.5E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-type">
      <value value="&quot;clearcut&quot;"/>
      <value value="&quot;shelterwood&quot;"/>
      <value value="&quot;single-tree&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-poplar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weevil-probability">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
