extensions [profiler]
globals [basal-area basal-area-ft qdbh qdbh-in dens dens-ac
  newsaps-maple newsaps-oak newsaps-poplar repro-oak repro-maple repro-poplar
  prop-oak prop-tol prop-intol]

breed [oaks oak]
oaks-own [Dmax Hmax Amax b2 b3 C G light-tol N-tol
  actual-growth intrinsic-mortality min-increment growth-mortality light degd-min degd-max wlmax wt-dist-min
  age dbh height canopy-radius canopy-density ba acorn-mean seedling potential-sapling sprout?
  fAL fT fWT fN fR
  
  tot-acorns
  ]

breed [maples maple]
maples-own [Dmax Hmax Amax b2 b3 C G light-tol N-tol
  actual-growth intrinsic-mortality min-increment growth-mortality light degd-min degd-max wlmax wt-dist-min
  age dbh height canopy-radius canopy-density ba seedling sprout?
  fAL fT fWT fN fR max-saplings potential-sapling
  ]

breed [poplars poplar]
poplars-own [Dmax Hmax Amax b2 b3 C G light-tol N-tol
  actual-growth intrinsic-mortality min-increment growth-mortality light degd-min degd-max wlmax wt-dist-min
  age dbh height canopy-radius canopy-density ba seedling sprout?
  fAL fT fWT fN fR max-saplings potential-sapling
  ]

breed [acorns acorn]
acorns-own [weevil cached germ]

patches-own [cc5 cc10 cc15 cc20 cc25 cc30 cc35 dn5 dn10 dn15]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup
  clear-all
  reset-ticks
  
  ask patches [reset-patches]
  
  ;;Initial stand values based on Saunders and Arseneault 2013
  
  create-oaks init-overstory-oak [
    set shape "tree"
    set size 2 
    set color black
    set seedling FALSE
    init-params
    ;;set dbh max list 0.015 ((random-normal 39.2 11.5) / 100)
    ;;set age random 20 + (initial-stand-age - 10)
    ;set dbh max list 0.015 ((random-normal 29.2 11.5) / 100)
    set dbh (random-normal 45.75 5) / 100
    set age random-normal 100 5 
    set ba (pi * (dbh / 2) ^ 2)
    set height (convert-dbh-height (dbh * 100)) / 100
    set canopy-radius convert-height-canopy-radius height
    set canopy-density calc-shade (dbh * 100)
      
    ifelse init-overstory-oak > 0 [
      loop [
        setxy random-xcor random-ycor
        if count oaks in-radius 7 < 2 [stop]]]
    [setxy random-xcor random-ycor]


  ]
  
  create-maples init-overstory-maple [
    set shape "leaf"
    set size 2 
    set color blue
    init-params
    ;;set dbh max list 0.015 ((random-normal 39.2 11.5) / 100)
    ;;set age random 20 + (initial-stand-age - 10)
    ;set dbh max list 0.015 ((random-normal 29.2 11.5) / 100)
    set dbh (random-normal 40.8 5) / 100
    set age round random-normal 100 5
    set ba (pi * (dbh / 2) ^ 2)
    set height (convert-dbh-height (dbh * 100)) / 100
    set canopy-radius convert-height-canopy-radius height
    set canopy-density calc-shade (dbh * 100)
      
    ifelse init-overstory-oak > 0 [
      loop [
        setxy random-xcor random-ycor
        if count turtles in-radius 8 < 2 [stop]]]
    [setxy random-xcor random-ycor]


  ]
  
  create-poplars init-overstory-poplar [
    set shape "plant"
    set size 2 
    set color violet
    init-params
    ;;set dbh max list 0.015 ((random-normal 39.2 11.5) / 100)
    ;;set age random 20 + (initial-stand-age - 10)
    ;set dbh max list 0.015 ((random-normal 29.2 11.5) / 100)
    set dbh (random-normal 45.07 5) / 100
    set age round random-normal 100 5
    set ba (pi * (dbh / 2) ^ 2)
    set height (convert-dbh-height (dbh * 100)) / 100
    set canopy-radius convert-height-canopy-radius height
    set canopy-density calc-shade (dbh * 100)
      
    ifelse init-overstory-oak > 200 [
      loop [
        setxy random-xcor random-ycor
        if count turtles in-radius 8 < 2 [stop]]]
    [setxy random-xcor random-ycor]


  ]
  
  
  ask turtles with [seedling = FALSE] [draw-canopy]
  
  ask patches [draw-final-canopy]
  
  ask turtles [calc-available-light]
  
  create-oaks init-sapling-oak [
    set shape "square"
    set size 1 
    set color black
    set seedling FALSE
    init-params
    set age round random-float 10
    set dbh max list 0.015 ((random-normal 14.9 5) / 100)
    set ba (pi * (dbh / 2) ^ 2)
    set height (convert-dbh-height (dbh * 100)) / 100
    set canopy-radius convert-height-canopy-radius height
    set canopy-density calc-shade (dbh * 100)
      
    loop [
      setxy random-xcor random-ycor
      if mean [cc10] of patches in-radius 2 < 0.5 [stop]]

  ]
  
  create-maples init-sapling-maple [
    set shape "square"
    set size 1 
    set color blue
    set seedling FALSE
    init-params
    set age round random-float 20
    set dbh max list 0.015 ((random-normal 10.3 5) / 100)
    set ba (pi * (dbh / 2) ^ 2)
    set height (convert-dbh-height (dbh * 100)) / 100
    set canopy-radius convert-height-canopy-radius height
    set canopy-density calc-shade (dbh * 100)
    setxy random-xcor random-ycor
  ]
  
  create-poplars init-sapling-poplar [
    set shape "square"
    set size 1 
    set color violet
    set seedling FALSE
    init-params
    set age round random-float 10
    set dbh max list 0.015 ((random-normal 14.9 5) / 100)
    set ba (pi * (dbh / 2) ^ 2)
    set height (convert-dbh-height (dbh * 100)) / 100
    set canopy-radius convert-height-canopy-radius height
    set canopy-density calc-shade (dbh * 100)
    loop [
      setxy random-xcor random-ycor
      if mean [cc10] of patches in-radius 2 < 0.5 [stop]]
  ]
  
  ask patches [reset-patches]
  
  ask turtles with [seedling = FALSE] [draw-canopy]
  
  ask patches [draw-final-canopy]
  
  ask oaks [calc-available-light]
  
  ;;color patches accordingly
  ask patches [color-patches]
  
  ;;Calculate global reporter values
  set basal-area sum [ba] of turtles with [height > 1.37]
  set basal-area-ft basal-area * 4.356
  set qdbh sqrt(basal-area / (0.0000785 * count turtles with [height > 1.37]))
  set qdbh-in 2 * (sqrt(mean [ba] of turtles with [height > 1.37] / pi)) * 39.37
  set dens (count turtles with [height >= 1.37])
  set dens-ac (count turtles with [height >= 1.37] * 0.40477)
  set prop-oak (sum [ba] of turtles with [height > 1.37 and breed = oaks] / basal-area)
  set prop-tol (sum [ba] of turtles with [height > 1.37 and breed = maples] / basal-area)
  set prop-intol (sum [ba] of turtles with [height > 1.37 and breed = poplars] / basal-area)
  setup-plots
 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  
  ask patches [reset-patches]
  set newsaps-oak 0
  set newsaps-maple 0
  set newsaps-poplar 0
  
  ask turtles with [seedling = FALSE] [draw-canopy]
  ask patches [draw-final-canopy]
  
  ;;color patches accordingly
  
  ask patches [color-patches]
  
  ask turtles with [seedling = FALSE] [
    calc-available-light
    grow
    check-survival
    if dbh >= 0.25 [reproduce]
  ]
  
  ;ask oaks with [dbh >= 0.2][produce-mast]
  ;ask turtles with [breed != oaks and dbh >= 0.2][regen-saplings]
  
  ask acorns [
   disperse-mast
   germinate-mast 
  ]
  
  ask turtles with [potential-sapling = TRUE] [check-sapling-existence]
  
  ask oaks with [seedling = TRUE] [
    calc-seedling-light
    grow-seedling
    check-seedling-survival
  ]
  
  ;;Calculate global reporter values
  set basal-area sum [ba] of turtles with [height > 1.37]
  set basal-area-ft basal-area * 4.356
  set qdbh sqrt(basal-area / (0.0000785 * count turtles with [height > 1.37]))
  set qdbh-in 2 * (sqrt(mean [ba] of turtles with [height > 1.37] / pi)) * 39.37
  set dens (count turtles with [height >= 1.37])
  set dens-ac (count turtles with [height >= 1.37] * 0.40477)
  if count oaks with [dbh >= 0.25] > 0 [set repro-oak (newsaps-oak / (count oaks with [dbh >= 0.25]))]
  if count maples with [dbh >= 0.25] > 0 [set repro-maple (newsaps-maple / (count maples with [dbh >= 0.25]))]
  if count poplars with [dbh >= 0.25] > 0 [set repro-poplar (newsaps-poplar / (count poplars with [dbh >= 0.25]))]
  set prop-oak (sum [ba] of turtles with [height > 1.37 and breed = oaks] / basal-area)
  set prop-tol (sum [ba] of turtles with [height > 1.37 and breed = maples] / basal-area)
  set prop-intol (sum [ba] of turtles with [height > 1.37 and breed = poplars] / basal-area)
  
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;Allometric equations;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report convert-dbh-height [dbh-input]  
  
  ;;Based on JABOWA, Botkin 1993
  ;;b2 = 2*(Hmax - 137) / Dmax
  ;;b3 = (Hmax - 137) / Dmax ^2
    
  report (137 + b2 * dbh-input - b3 * (dbh-input ^ 2))  
    
end
  
  
to-report convert-height-canopy-radius [height-input]
  
  ;;Based on Kenefic and Nyland 1999 and others
  ;;range reported is 0.376 - 0.393; going with .385 for now
  
  ;report (0.4 * height-input / 2)
  report max list 1 (0.5 * height-input / 2)
  
end

to-report calc-shade [dbh-input]
  
  ;;leaf weight based on dbh
  ;;C is species-specific parameter
  ;;Based on Botkin 1992/1993
  ;;adjusted exponent to 2.5
  ;;Botkin notes it can be anywhere from 1.5 to 3
  ;;let SLA (C * dbh-input ^ (2.5))
  let SLA (C * dbh-input ^ (2.4))
  
  ;;Beer-Lambert law where light above canopy = 1
  ;;K set according to QuabbinPlot206
  let k 0.0000756
  ;let k 0.00016666667
  let PHI 1
  report min list 0.98 (1 - (PHI * exp(-1 * k * SLA)))
    
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;Growth equations;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Seedlings go up to height 137 cm (1.37 m)
;;After that dbh is the primary measurement

to-report max-growth-increment [dbh-input height-input]
  
  ;;Based on JABOWA, Botkin 1993
  
  let num (G * dbh-input * ((1 - (dbh-input * (137 + (b2 * dbh-input) - (b3 * dbh-input ^ 2))) / (Dmax * Hmax))))
  let denom (274 + (3 * b2 * dbh-input) - (4 * b3 * dbh-input ^ 2))
  report (num / denom)
  
end

to-report light-growth-index [tol-input]
  
  ;Based on Botkin 1993 and Bonan 1990
  if tol-input = "low" [    
    report (2.24 * (1 - exp(-1.136 * (light - 0.08))))       
  ]
  
  if tol-input = "intermediate" [
    report (1.371 * (1 - exp(-2.227 * (light - 0.05))))
  ]
  
  if tol-input = "high" [
    report (1 - exp(-4.64 * (light - 0.05)))   
  ]
  
end

to-report degree-days-index [degd-min-input degd-max-input]
  
  let tdegd (4 * (DegDays - degd-min-input) * (degd-max-input - DegDays) / ((degd-max-input - degd-min-input) ^ 2))
  
  report max list 0 tdegd
    
end

to-report saturation-index [wt-dist-min-input]
  
  let wefi (1 - (wt-dist-min-input / wt-dist))
  
  report max list 0 wefi
  
end


to-report nitrogen-index [N-tol-input]
  
  if N-tol-input = "intolerant" [
    let a1 2.99 let a2 0.00175 let a3 207.43 let a4 -5 let a5 2.9 let a6 3.671
    let lambdaN (a1 * (1 - 10 ^ (-1 * a2 * (available-N + a3))))
    report (a4 + a5 * lambdaN) / a6
    ]
  if N-tol-input = "intermediate" [
    let a1 2.94 let a2 0.00234 let a3 117.52 let a4 -1.2 let a5 1.3 let a6 2.622
    let lambdaN (a1 * (1 - 10 ^ (-1 * a2 * (available-N + a3))))
    report (a4 + a5 * lambdaN) / a6
    ]
  if N-tol-input = "tolerant" [
    let a1 2.79 let a2 0.00179 let a3 219.77 let a4 -0.6 let a5 1.0 let a6 2.190
    let lambdaN (a1 * (1 - 10 ^ (-1 * a2 * (available-N + a3))))
    report (a4 + a5 * lambdaN) / a6
    ]
  
end




to grow

  let max-growth (max-growth-increment (dbh * 100) (height * 100))
  
  set fAL light-growth-index light-tol
  
  set fT degree-days-index degd-min degd-max
  
  set fWT saturation-index wt-dist-min
  
  set fN nitrogen-index N-tol
  
  ;;random variability
  set fR (0.95 + (random-float 0.1))
    
  set actual-growth (max-growth * fAL * fT * fWT * fN * fR)
  
  set dbh (dbh * 100 + actual-growth) / 100
  set height (convert-dbh-height (dbh * 100)) / 100
  set canopy-radius (convert-height-canopy-radius height)
  set canopy-density (calc-shade (dbh * 100))
  set ba (pi * (dbh / 2) ^ 2)
  
  if dbh >= 0.1 and shape = "square" [
    if breed = oaks [set size 2 set shape "tree"]
    if breed = maples [set size 2 set shape "leaf"]
    if breed = poplars [set size 2 set shape "plant"]
  ]
  
end

to check-survival
  
  if dbh >= 0.1 [ 
    if random-float 1 < intrinsic-mortality [create-sprout] 
    if actual-growth < min-increment and random-float 1 < growth-mortality [create-sprout]
  ]
  
  if dbh < 0.1 [
    if random-float 1 < intrinsic-mortality [die] 
    if actual-growth < min-increment and random-float 1 < growth-mortality [die]
  ]
  
  set age age + 1
  
end


to create-sprout
  ;;From Dey 2002
  if breed = oaks [
    let prob (1 + exp(-1 * (5.9991 - 0.2413 * (dbh * 39.3701) - 0.0475 * age))) ^ (-1)
    ifelse random-float 1 < prob [
      set shape "square" set color black   
      set age 1
      set dbh 0.01
      set height (convert-dbh-height (dbh * 100)) / 100
      set canopy-radius (convert-height-canopy-radius height)
      set canopy-density (calc-shade (dbh * 100))
      set ba (pi * (dbh / 2) ^ 2)      
    ]
    [die]
  ]
  if breed = maples [
    ;;Based on Powell 1983
    let prob -0.314 * ln(dbh) + 0.0877
    if prob > 1 [set prob 1]
    if dbh > 0.8 [set prob 0]
    ifelse random-float 1 < prob [
      set shape "square" set color blue
      set age 1
      set dbh 0.01
      set height (convert-dbh-height (dbh * 100)) / 100
      set canopy-radius (convert-height-canopy-radius height)
      set canopy-density (calc-shade (dbh * 100))
      set ba (pi * (dbh / 2) ^ 2)          
    ]
    [die]
  ]
  if breed = poplars [
    ;;based on Wendel 1975 plus some guesses
    let prob 0.97
    if dbh > 0.8 [set prob 0]
    ifelse random-float 1 < prob [
      set shape "square" set color violet set sprout? TRUE
      set dbh 0.01
      set height (convert-dbh-height (dbh * 100)) / 100
      set canopy-radius (convert-height-canopy-radius height)
      set canopy-density (calc-shade (dbh * 100))
      set age 1
    ]
    [die]
    ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Processes associated with reproduction;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to reproduce 
  
  ifelse breed = oaks [
    let acorns-produced (light * 3.1415 * canopy-radius * canopy-radius * random-poisson acorn-mean) ;per meter squared
    let tree-radius canopy-radius
    ;let treexcor xcor
    ;let treeycor ycor
    hatch-acorns acorns-produced [
      ;;drop produced acorns under canopy randomly 
      set hidden? TRUE
      right random 360
      forward (random-float (tree-radius - 0.5)) + 0.5
      ;set xcor (treexcor + random tree-radius - random tree-radius - 0.5)
      ;set ycor (treeycor + random tree-radius - random tree-radius - 0.5)
      ifelse random-float 1 < weevil-probability [set weevil TRUE] [set weevil FALSE] ;;check to see if weeviled
    ]
  ]
  ;other tree species
  [
   let max-sap max-saplings
   hatch (random (max-saplings + 1)) [
     set hidden? TRUE
     init-params
     right random 360
     ;;based on HEE data
     forward random-exponential 5.185 
     set potential-sapling TRUE
     ;set xcor (treexcor + random tree-radius - random tree-radius - 0.5)
     ;set ycor (treeycor + random tree-radius - random tree-radius - 0.5)   
     ;right random 360
     ;;based on HEE data
     ;forward random-exponential 5.185 
     check-sapling-existence   
   ]
  ]
  
  
end

to disperse-mast
  ;;move mast via "dispersers"
  ;;removal probability - HEE dispersal data for WO
  ifelse random-float 1 < 0.41 and weevil = FALSE [
    right random 360
    ;;based on HEE data
    forward random-exponential 5.185
    ;;probability of being eaten
    ifelse random-float 1 > 0.704 [
      ;;probability of being cached
      ifelse random-float 1 < 0.288 [set cached TRUE] [set cached FALSE]]
    [die]]
  ;;check if eaten
  [if random-float 1 < 0.538 [die]]
end


to germinate-mast
  ifelse cached = TRUE [
    set germ germ-prob]
  ;;placeholder
  [ifelse weevil = TRUE [set germ germ-prob * 0.1 * 0.1] 
    [set germ germ-prob * 0.1]]  
  if random-float 1 < germ [
      hatch-oaks 1 [
        set age 1
        set size 1
        set seedling TRUE   
        set hidden? TRUE
      ]]
    die
end

to check-sapling-existence
  calc-available-light
  set fAL light-growth-index light-tol 
  set fT degree-days-index degd-min degd-max  
  set fWT saturation-index wt-dist-min 
  set fN nitrogen-index N-tol
  
  ifelse random-float 1 > (fAL * fT * fWT * fN) [die] [
    set potential-sapling FALSE
    
    set size 1
    set shape "square"
    if breed = maples [set newsaps-maple (newsaps-maple + 1)]
    if breed = poplars [set newsaps-poplar (newsaps-poplar + 1)]
    set hidden? FALSE
    set age 1
    set dbh 0.01  
    set ba (pi * (dbh / 2) ^ 2)
    set height (convert-dbh-height (dbh * 100)) / 100
    set canopy-radius convert-height-canopy-radius height
    set canopy-density calc-shade (dbh * 100) 
    
    
    ]    
  
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Seedling-specific processes;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to calc-seedling-light
  set light (1 - mean [cc5] of patches in-radius 2)
end

to grow-seedling
  ;growth is height-based
  let max-growth seedling-growth 
  set fAL light-growth-index "intermediate"
  set actual-growth (max-growth * fAL)
  set height (height + actual-growth)
  
  if height > 1.37 [
    init-params
    set newsaps-oak (newsaps-oak + 1)
    set dbh (1 + random-float 0.5) / 100
    set seedling FALSE
    set hidden? FALSE
    set shape "square"
  ]
end

to check-seedling-survival
  ;Placeholders
  ;Intrinsic seedling mortality
  if random-float 1 < 0.1 [die]
  
  ;Growth-based mortality
  if actual-growth < (0.5 * seedling-growth) and random-float 1 < 0.393 [die]
  
  set age age + 1
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Processes associated with available light;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to draw-canopy

  ;;canopy leaf density for this individual based on species and dbh
  let temp canopy-density
  ;;add this tree's canopy to the canopy density w/in its canopy radius
  
  ifelse height < 5 [
    ask patches in-radius canopy-radius [set cc5 (cc5 + (1 - cc5) * temp)]
    ask patches in-radius 2 [set dn5 (dn5 + 1)]
  ] [
    ifelse height >= 5 and height < 10 [
      ask patches in-radius canopy-radius [set cc10 (cc10 + (1 - cc10) * temp)]
      ask patches in-radius 2 [set dn10 (dn10 + 1)]
    ] [
      ifelse height >= 10 and height < 15 [
        ask patches in-radius canopy-radius [set cc15 (cc15 + (1 - cc15) * temp)]
        ask patches in-radius 2 [set dn15 (dn15 + 1)]
      ] [
        ifelse height >= 15 and height < 20 [
          ask patches in-radius canopy-radius [set cc20 (cc20 + (1 - cc20) * temp)]
        ] [
          ifelse height >= 20 and height < 25 [
            ask patches in-radius canopy-radius [set cc25 (cc25 + (1 - cc25) * temp)]
          ] [
            ifelse height >= 25 and height < 30 [
              ask patches in-radius canopy-radius [set cc30 (cc30 + (1 - cc30) * temp)]
            ] [
              if height >= 30 [
                ask patches in-radius canopy-radius [set cc35 (cc35 + (1 - cc35) * temp)]
              ]]]]]]]
 
end


to draw-final-canopy
  
  set cc30 (cc30 + (1 - cc30) * cc35)
  set cc25 (cc25 + (1 - cc25) * cc30)
  set cc20 (cc20 + (1 - cc20) * cc25)
  set cc15 (cc15 + (1 - cc15) * cc20)
  set cc10 (cc10 + (1 - cc10) * cc15)
  set cc5  (cc5  + (1 - cc5 ) * cc10)
  
end


to calc-available-light
  ifelse height < 5 [
    set light (1 - mean [cc10] of patches in-radius 3) / (max list 1 ([dn5] of patch-here))
  ] [
    ifelse height >= 5 and height < 10 [
      set light (1 - mean [cc15] of patches in-radius 3) / (max list 1 ([dn10] of patch-here))
    ] [
      ifelse height >= 10 and height < 15 [
        set light (1 - mean [cc20] of patches in-radius canopy-radius) / (max list 1 ([dn15] of patch-here))
      ] [
        ifelse height >= 15 and height < 20 [
          set light (1 - mean [cc25] of patches in-radius canopy-radius)
        ] [
          ifelse height >= 20 and height < 25 [
            set light (1 - mean [cc30] of patches in-radius canopy-radius)
          ] [
            ifelse height >= 25 and height < 30 [
              set light (1 - mean [cc35] of patches in-radius canopy-radius)
            ] [
              ifelse height >= 30 and height >= max [height] of turtles with [breed != acorns] in-radius canopy-radius [    
               set light 1
              ] [set light (1 - mean [cc35] of patches in-radius canopy-radius)]
              
              ]]]]]]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Non-oak Regeneration;;;;;;;:::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to init-params
  if breed = oaks [
    ;White oak; Based on Botkin 1993 and Holm 2012
    set Dmax 100
    set Hmax 3500
    set Amax 400
    set b2 2 * (Hmax - 137) / (Dmax)
    set b3 (Hmax - 137) / (Dmax ^ 2)
    set C 1.75
    set G 104
    set degd-min 1977
    set degd-max 5894
    set wt-dist-min 0.933
    set wlmax 0.45
    set light-tol "intermediate"
    set N-tol "intermediate"
    set actual-growth 0.02
    ;Based on 2% reaching max age; common to all species
    ;Based on Botkin 1993
    set intrinsic-mortality 4.0 / Amax
    ;set min-increment 0.01
    set min-increment 0.01
    set growth-mortality 0.368
    ;;mean oak production based on HEE histogram
    set acorn-mean random-exponential 10
    set potential-sapling FALSE
  ]
  if breed = maples [
    ;Sugar maple; based bon Botkin 1993 and Holm 2012
    ;set Dmax 170
    set Dmax 100
    set Hmax 3350
    set Amax 400
    set b2 2 * (Hmax - 137) / (Dmax)
    set b3 (Hmax - 137) / (Dmax ^ 2)
    set C 1.57
    set G 118.7
    set degd-min 2000
    set degd-max 6300
    set wt-dist-min 0.567
    set wlmax 0.35
    set light-tol "high"
    set N-tol "intermediate"
    set actual-growth 0.02
    ;Based on 2% reaching max age; common to all species
    ;Based on Botkin 1993
    set intrinsic-mortality 4.0 / Amax
    ;set min-increment 0.01
    set min-increment 0.01
    set growth-mortality 0.368
    set seedling FALSE
    set potential-sapling FALSE
    ;Based on Botkin 1993
    set max-saplings 4
  ]
  if breed = poplars [
    ;Tulip poplar; based bon Botkin 1993 and Holm 2012
    set Dmax 100
    set Hmax 4000
    set Amax 300
    set b2 2 * (Hmax - 137) / (Dmax)
    set b3 (Hmax - 137) / (Dmax ^ 2)
    ;assumed to be similar to oak
    set C 1.75
    ;;;;;
    set G 140
    set degd-min 2171
    set degd-max 6363
    ;;based on white spruce/red maple (similar moisture tolerance)
    set wt-dist-min 0.544
    set wlmax 0.245
    ;;;;;;;;
    set light-tol "low"
    set N-tol "intermediate"
    set actual-growth 0.02
    ;Based on 2% reaching max age; common to all species
    ;Based on Botkin 1993
    set intrinsic-mortality 4.0 / Amax
    ;set min-increment 0.01
    set min-increment 0.01
    set growth-mortality 0.368
    set seedling FALSE
    set potential-sapling FALSE
    ;Based on Botkin 1993
    set max-saplings 4
  ]
    
end

to color-patches
  if cc5 > 0.1 [set pcolor green]
  if cc5 > 0.5 [set pcolor lime]
  if cc5 > 0.7 [set pcolor yellow]
  if cc5 > 0.85 [set pcolor orange]
  if cc5 > 0.9 [set pcolor red]
end

to reset-patches
  set pcolor brown
  set cc5 0
  set cc10 0
  set cc15 0
  set cc20 0
  set cc25 0
  set cc30 0
  set cc35 0
  set dn5 0
  set dn10 0
  set dn15 0
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
1028
849
50
50
8.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
1
1
1
ticks
30.0

BUTTON
27
21
90
54
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
14
100
195
133
init-overstory-oak
init-overstory-oak
48
123
89
1
1
trees
HORIZONTAL

BUTTON
97
21
160
54
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
1039
168
1214
201
DegDays
DegDays
1980
5500
4444
1
1
NIL
HORIZONTAL

SLIDER
1040
206
1212
239
wt-dist
wt-dist
0.1
10
6.9
0.1
1
m
HORIZONTAL

SLIDER
1040
246
1212
279
available-N
available-N
0
350
300
25
1
kg/ha/yr
HORIZONTAL

MONITOR
25
380
93
425
BA (m2/ha)
basal-area
2
1
11

SLIDER
1039
13
1211
46
weevil-probability
weevil-probability
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
1038
54
1210
87
germ-prob
germ-prob
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
1038
96
1210
129
seedling-growth
seedling-growth
0
1.37
0.6
0.01
1
m
HORIZONTAL

MONITOR
1045
297
1109
342
seedlings
count oaks with [seedling = TRUE]
0
1
11

SLIDER
14
138
186
171
init-sapling-oak
init-sapling-oak
37
124
95
1
1
NIL
HORIZONTAL

SLIDER
14
178
201
211
init-overstory-maple
init-overstory-maple
0
22
11
1
1
trees
HORIZONTAL

MONITOR
25
431
92
476
qDBH (cm)
qdbh
2
1
11

MONITOR
100
380
174
425
BA (ft2/ac)
basal-area-ft
2
1
11

MONITOR
103
431
168
476
qDBH (in)
qdbh-in
2
1
11

MONITOR
27
483
91
528
Trees/Ha
dens
2
1
11

MONITOR
104
483
167
528
Trees/Ac
dens-ac
2
1
11

PLOT
1043
359
1243
509
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

MONITOR
26
552
91
597
Oak Repro
repro-oak
2
1
11

MONITOR
101
552
172
597
Maple Repro
repro-maple
2
1
11

PLOT
1044
518
1244
668
Prop Oak
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"set-plot-y-range (precision 0 0) (precision 1 0)\nplot-pen-up\nplot-pen-down" ""
PENS
"default" 1.0 0 -16777216 true "" "plot prop-oak"
"pen-1" 1.0 0 -13345367 true "" "plot prop-tol"
"pen-2" 1.0 0 -6917194 true "" "plot prop-intol"

SLIDER
14
219
186
252
init-overstory-poplar
init-overstory-poplar
0
18
9
1
1
NIL
HORIZONTAL

MONITOR
26
605
98
650
Poplar Repro
repro-poplar
2
1
11

SLIDER
16
263
188
296
init-sapling-maple
init-sapling-maple
0
700
538
1
1
NIL
HORIZONTAL

SLIDER
16
303
188
336
init-sapling-poplar
init-sapling-poplar
51
354
162
1
1
NIL
HORIZONTAL

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
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
