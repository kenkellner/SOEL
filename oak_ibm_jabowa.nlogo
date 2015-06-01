extensions [profiler]
globals [basal-area basal-area-ft qdbh qdbh-in dens dens-ac
  prop-oak prop-tol prop-intol
  sitequal-boak sitequal-woak sitequal-maple sitequal-poplar
  harvest-year shelter-phase
  ba-oak ba-map ba-pop
  ]

breed [oaks oak]
oaks-own [species light age dbh height actual-growth ba lai fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality 
  ]

breed [maples maple]
maples-own [light age dbh height actual-growth ba lai fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality
  ]

breed [poplars poplar]
poplars-own [light age dbh height actual-growth ba lai fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality
  ]

patches-own [plight]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to setup
  clear-all
  reset-ticks
  
  let xcutoff (x-core / 2)
  let ycutoff (y-core / 2)
  
  let adjust (x-core + buffer * 2) * (y-core + buffer * 2) / 100
  
  resize-world 0 (x-core + 2 * buffer - 1) 0 (y-core + 2 * buffer - 1)
  ;resize-world (-1 * (xcutoff + buffer - 1)) (xcutoff + buffer) (-1 * (ycutoff + buffer - 1)) (ycutoff + buffer)
  
  calc-site-quality
 
  if HEE-mean = TRUE [
    init-stand adjust TRUE 89 11 9 95 499 163 
    calc-global-vars 
    setup-plots  
  ] 
  
  ask patches [color-patches]
  set harvest-year burnin
  set shelter-phase 1
  
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  
  ask patches [
    regenerate
    color-patches   
  ]

  ask turtles [
    calc-light
  ]
  
  ask turtles [
    grow
    check-survival
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
  ;leaf weight based on dbh with C species-specific
  ;Exponent can be anywhere from 1.5 to 3 (Botkin 1993)
  set lai (C * (dbh-input * 100) ^ (2))  
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
  
  set actual-growth (max-growth * fAL * sitequal)
  set dbh (dbh * 100 + actual-growth) / 100 
  allometerize dbh
  
  if dbh >= 0.1 and shape = "square" [
    if breed = oaks [set size 1 set shape "tree"]
    if breed = maples [set size 1 set shape "tree"]
    if breed = poplars [set size 1 set shape "tree"]
  ]
  
end


to check-survival
  
  if random-float 1 < intrinsic-mortality [die] 
  if actual-growth < min-increment and random-float 1 < growth-mortality [die]
  set age age + 1
  
end

to-report patch-light
  
  let k light-extinct
  let canopy turtles-here
  report Exp (-1 * sum [lai * k] of canopy);]
  
end

to regenerate
  
  set plight patch-light
  
  let AL light-growth-index plight "high"
  let Al-pop light-growth-index plight "low"
  let Al-oak light-growth-index plight "intermediate"
  let max-maple-saplings 3 let max-poplar-saplings 10 let max-oak-saplings 10
  
  ;Shade intermediate-tolerant species
  if plight >= 0.5 and plight < 0.99 [    
    let rw random-float 1
    let rb random-float 1
    
    if rw < (Al-oak * sitequal-woak)[    
      let woak-spawn (max-oak-saplings * rw)
      sprout-oaks (round woak-spawn)[
        set dbh random-float 0.0041
        set species "WO"
        init-params
      ]]
    
    if rb < (Al-oak * sitequal-boak)[
      let boak-spawn (max-oak-saplings * rb)
      sprout-oaks (round boak-spawn)[
        set dbh random-float 0.0041
        set species "BO"
        init-params
      ]]
  ]
  
  ;Shade tolerant species
  let rm random-float 1
  if rm < (Al * sitequal-maple)[
    sprout-maples round (max-maple-saplings * rm)[
      set dbh random-float 0.00468
      init-params
    ]]
  
  ;Shade intolerant species
  if plight >= 0.99 and sitequal-poplar > 0 [    
    let pop-spawn (random-float 1 * max-poplar-saplings * Al-pop * sitequal-poplar)    
    sprout-poplars (round pop-spawn) [
      set dbh random-float 0.0039   
      init-params     
  ]]
          
end


to calc-light
  let currh height
  ifelse currh >= max [height] of turtles-here [set hidden? FALSE][set hidden? TRUE]
  let k light-extinct
  let canopy turtles-here with [height > currh]
  set light Exp (-1 * sum [lai * k] of canopy);]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                       Utility Procedures                        ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
        setxy random-pxcor random-pycor
        if count turtles-here < 2 [stop]]]
    [setxy random-pxcor random-pycor]
  ]  
  create-maples dens-adjust * n-maple [
    set dbh (random-normal 40.8 5) / 100
    init-params
    set age round (Amax * (dbh * 100) / Dmax)      
    ifelse space-adjust = TRUE [
      loop [
        setxy random-pxcor random-pycor
        if count turtles-here < 2 [stop]]]
    [setxy random-pxcor random-pycor]
  ] 
  create-poplars dens-adjust * n-poplar [
    set dbh (random-normal 45.07 5) / 100
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    ifelse space-adjust = TRUE [
      loop [
        setxy random-pxcor random-pycor
        if count turtles-here < 3 [stop]]]
    [setxy random-pxcor random-pycor]
  ]
  
  ask turtles [calc-light]
  ask patches [set plight patch-light]
  
  ;;Create saplings
  create-oaks dens-adjust * n-sap-oak [
    ifelse random-float 1 > 0.5 [set species "WO"][set species "BO"]
    set dbh max list 0.015 ((random-normal 14.9 5) / 100)
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    loop [
      setxy random-pxcor random-pycor
      if [plight] of patch-here > 0.6 [stop]]
  ]  
  create-maples dens-adjust * n-sap-maple [
    set dbh max list 0.015 ((random-normal 10.3 5) / 100)
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    setxy random-pxcor random-pycor
  ] 
  create-poplars dens-adjust * n-sap-poplar [
    set dbh max list 0.015 ((random-normal 14.9 5) / 100)
    init-params
    set age round (Amax * (dbh * 100) / Dmax)
    loop [
      setxy random-pxcor random-pycor
      if [plight] of patch-here > 0.6 [stop]]
  ]
  
  ask turtles [calc-light]
  ask patches [set plight patch-light]
   
end



to init-params
  ifelse dbh < 0.1 [set shape "square"] [set shape "tree"]
  set hidden? FALSE
  set actual-growth 0.02
  set min-increment 0.01
  set growth-mortality 0.369
  set age 1
  if breed = oaks [ 
    ifelse species = "WO" [;White oak based on Botkin 1993 and Holm 2012
      set color white
      set Dmax 100 set Hmax 3800 ;based on HEE data
      set Amax 400 set intrinsic-mortality 4.0 / Amax ;Based on 2% reaching max age; common to all species
      set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
      set C 1.75
      set G 104
      set light-tol "intermediate"      
    ][set color black ;Black oak based on Botkin 1993 and Holm 2012
      set Dmax 100 set Hmax 3800 ;based on HEE data
      set Amax 300 set intrinsic-mortality 4.0 / Amax ;Based on 2% reaching max age; common to all species
      set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
      set C 1.75
      set G 122
      set light-tol "intermediate"
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

  let xmin buffer
  let xmax buffer + x-core - 1  
  let ymin buffer
  let ymax buffer + y-core - 1
    
  let adjust (x-core * y-core) / 100
  
  set basal-area (sum [ba] of turtles with [dbh >= 0.01 
      and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
  set basal-area-ft basal-area * 4.356
  set qdbh sqrt(basal-area * adjust / (0.0000785 * count turtles with [height > 1.37 
        and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]))
  set qdbh-in 2 * (sqrt(mean [ba] of turtles with [height > 1.37 
        and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin] / pi)) * 39.37
  set dens (count turtles with [dbh >= 0.01 
     and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
  set dens-ac dens * 0.40477
  set ba-oak (sum [ba] of turtles with [dbh >= 0.01 and breed = oaks 
      and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
  set ba-map (sum [ba] of turtles with [dbh >= 0.01 and breed = maples 
      and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
  set ba-pop (sum [ba] of turtles with [dbh >= 0.01 and breed = poplars 
      and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
  if basal-area > 0 [
    set prop-oak (ba-oak / basal-area)
    set prop-tol (ba-map / basal-area)
    set prop-intol (ba-pop / basal-area)
  ]
end

to color-patches
  if (1 - plight) > 0 [set pcolor lime + 1]
  if (1 - plight) > 0.1 [set pcolor lime]
  if (1 - plight) > 0.2 [set pcolor lime - 1]
  if (1 - plight) > 0.3 [set pcolor green + 2]
  if (1 - plight) > 0.5 [set pcolor green + 1]
  if (1 - plight) > 0.7 [set pcolor green]
  if (1 - plight) > 0.8 [set pcolor green - 1]
  if (1 - plight) > 0.9 [set pcolor green - 2]
  if (1 - plight) > 0.95 [set pcolor green - 3]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                    Harvesting Procedures                        ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to conduct-harvest
  
  if harvest-type = "none" [stop]
  
  if (ticks + 1) != harvest-year [stop]
    
  let xmin buffer
  let xmax buffer + x-core - 1  
  let ymin buffer
  let ymax buffer + y-core - 1
  
  let adjust (x-core * y-core) / 100
  
  if harvest-type = "clearcut" [
    ask turtles with [dbh > 0.01 and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin] [die]
    ;set harvest-year harvest-year + 100
  ]
  
  if harvest-type = "singletree" [
    set basal-area (sum [ba] of turtles with [dbh >= 0.01 
        and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
    set harvest-year harvest-year + 20 
    if basal-area >= 25 [
      loop [
        let potential one-of turtles with [dbh >= 0.10 and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]
        ask potential [die]
        set basal-area (sum [ba] of turtles with [dbh >= 0.01 
            and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
        if basal-area <= 25 [stop]
        ]
    ]
  ]
  
  if harvest-type = "shelterwood" [
    ifelse shelter-phase = 1 [
      set shelter-phase 2
      ask turtles with [breed != oaks and dbh <= 0.254 
        and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin] [die] ;treated with herbicide
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
            let potential min-one-of turtles with [age > 20 
              and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin] [light]
            ask potential [ifelse breed = oaks [die][die]]
            set basal-area (sum [ba] of turtles with [dbh >= 0.01 
              and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin]) / adjust
            if basal-area <= 16.1 [stop]
          ]
        ]      
      ] 
      [
        ask turtles with [age > 20 and xcor <= xmax and xcor >= xmin and ycor <= ymax and ycor >= ymin] [die]
        set shelter-phase 1
        ;set harvest-year (harvest-year + 100)
      ]
       
  ]]
  
end
@#$#@#$#@
GRAPHICS-WINDOW
228
21
798
772
-1
-1
40.0
1
10
1
1
1
0
1
1
1
0
13
0
17
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
944
239
1119
272
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
945
277
1117
310
wt-dist
wt-dist
0.1
10
8.4
0.1
1
m
HORIZONTAL

SLIDER
945
317
1117
350
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
32
226
100
271
BA (m2/ha)
basal-area
2
1
11

MONITOR
32
277
99
322
qDBH (cm)
qdbh
2
1
11

MONITOR
107
226
175
271
BA (ft2/ac)
basal-area-ft
2
1
11

MONITOR
110
277
175
322
qDBH (in)
qdbh-in
2
1
11

MONITOR
34
329
98
374
Trees/Ha
dens
2
1
11

MONITOR
111
329
174
374
Trees/Ac
dens-ac
2
1
11

PLOT
7
380
207
530
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
8
539
208
689
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

SWITCH
966
419
1078
452
HEE-mean
HEE-mean
0
1
-1000

TEXTBOX
987
401
1137
419
Initial Forest\n
14
0.0
1

SLIDER
946
355
1118
388
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
909
190
986
235
NIL
sitequal-woak
2
1
11

MONITOR
989
190
1066
235
NIL
sitequal-maple
2
1
11

MONITOR
1069
190
1149
235
NIL
sitequal-poplar
2
1
11

TEXTBOX
980
39
1130
57
Site Conditions
14
0.0
1

TEXTBOX
82
207
232
225
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
1

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
943
489
1115
522
light-extinct
light-extinct
0.00016667
0.00033333
2.5E-4
0.00001
1
NIL
HORIZONTAL

TEXTBOX
970
468
1120
486
Tuning Parameters
14
0.0
1

SWITCH
958
64
1093
97
manual-site-qual
manual-site-qual
0
1
-1000

SLIDER
936
106
1028
139
sqwoak
sqwoak
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1032
106
1124
139
sqboak
sqboak
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
937
143
1029
176
sqmap
sqmap
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1032
143
1124
176
sqpop
sqpop
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
246
61
418
94
x-core
x-core
1
25
10
1
1
cells
HORIZONTAL

SLIDER
246
98
418
131
y-core
y-core
1
25
14
1
1
cells
HORIZONTAL

SLIDER
246
134
418
167
buffer
buffer
0
5
2
1
1
cells
HORIZONTAL

SLIDER
21
158
193
191
burnin
burnin
0
100
10
1
1
years
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
