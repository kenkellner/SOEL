globals [year harvestyear harvestsite phase Hs Ds J G basal-area prop-oak prop-tol prop-intol tree-dens]
breed [oaks oak]
breed [acorns acorn]
breed [maples maple]
breed [poplars poplar]
oaks-own [age dbh height canopy-radius canopy-density midstory-density ba light-cutoff sprout?
  root stage dominance light acorn-mean seedling-growth site-index
  b1 b2 b3 b4 b5 maxage maxht]
maples-own [age dbh height canopy-radius canopy-density midstory-density ba seedling-growth light-cutoff sprout?
  root stage dominance light site-index
  b1 b2 b3 b4 b5 maxage maxht]
poplars-own [age dbh height canopy-radius canopy-density midstory-density ba seedling-growth light-cutoff sprout?
  root stage dominance light site-index
  b1 b2 b3 b4 b5 maxage maxht]
acorns-own [weevil cached germ]
patches-own [canopy-cover midstory-cover]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;SETUP FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to setup
  clear-all
  reset-ticks
  ;;initialize some index variables related to harvest
  set year 0
  ifelse full-harvest = TRUE [set harvestyear 50]
  [set harvestyear rotation-length]
  set harvestsite 1
  set phase 1
  
  ;;calculate some values related to dbh/height
  ;;Johnson 2002
  ;;site age stabilized at 100 to avoid crashes
  set Hs 1.80408 * (site-index-oak * 0.3048) ^ (0.932097) * (1 - exp(-0.0240308 * 100)) ^ (2.55529 * (site-index-oak * 0.3048) ^ (-0.280445))
  set Ds 6.40146 * (site-index-oak * 0.3048) ^ (0.631893) * (1 - exp(-0.0227614 * 100)) ^ (1.21892)
  set J ln((1.12 * Hs - Hs) / (1.12 * Hs - 1.37)) / Ds
  set G 1.12 * Hs - 1.37
  
  
  ;;set all patches to no cover and color brown to initialize
  ask patches [
    set pcolor brown
    set midstory-cover 0
    set canopy-cover 0]
  
  ;;Create intial oaks:
  ;;Create mature oaks (even-aged distribution)
  create-oaks init-mature-oak [
    init-oak-values
    set age random-poisson stand-age
    set stage "mature"
    set shape "tree"
    set size 2
    ;;mean oak production based on HEE histogram
    set acorn-mean random-exponential 10
   
    ;; from Carmean 1989 - set height based on growth equation; set other size vars based on height
    set height (b1 * site-index ^ b2 * (1 - e ^ (b3 * (age + 1))) ^ (b4 * site-index ^ b5)) * 0.3048
    if height > maxht [set height maxht]
    ;;Kenefic and Nyland 1999
    set dbh (ln(1 - ((height - 1.37) / G)) / J) * 0.01
  
    ;;basal area
    set ba pi * (dbh / 2) ^ 2
    ;;From Yang et al. 1999
    set canopy-radius (0.376 * height) / 2
    
    ;;place trees semi-randomly (so they aren't unrealisticly close to each other)
    ifelse init-mature-oak < 500 [
    loop [
    setxy random-xcor random-ycor
    if count turtles with [stage = "mature"] in-radius 4 < 2 [stop]]]
    [setxy random-xcor random-ycor]
  ]
  
  ;;create sapling oaks
  create-oaks init-sapling-oak [
    init-oak-values
    set stage "sapling"
    set shape "square"
    set age random 32 + 3
    ;;from Carmean 1989 - set size based on age as above
    set height (b1 * site-index ^ b2 * (1 - e ^ (b3 * (age + 1))) ^ (b4 * site-index ^ b5)) * 0.3048
    set dbh (ln(1 - ((height - 1.37) / G)) / J) * 0.01
    set ba pi * (dbh / 2) ^ 2
    ;;set canopy-radius sqrt(height * 3.585 / pi)
    ;;set canopy-radius (1.7173 + dbh * 15.6159) / 2
    set canopy-radius (0.376 * height) / 2
    setxy random-xcor random-ycor
  ]
  
  ;;create seedling oaks
  create-oaks init-seedling-oak [
    init-oak-values
    set size 1
    set stage "seedling"
    set age random 3
    ;;seedlings are below 1.5 m in height
    set height random 1.5
    ;;will only grow if available light is above cutoff
    set light-cutoff 0.49
    ;;to minimize distractions
    set hidden? TRUE
    setxy random-xcor random-ycor
  ]
  
  ;;now create maple trees on site (basically same way as oak)
  create-maples init-mature-maple [
    init-maple-values
    set stage "mature"
    set age random-poisson stand-age
    set shape "leaf"
    set size 2
    ;;set size based on height and site index (equation is the same, parameters b1-b5 differ)
    set height (b1 * site-index ^ b2 * (1 - e ^ (b3 * (age + 1))) ^ (b4 * site-index ^ b5)) * 0.3048
    set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
    set ba pi * (dbh / 2) ^ 2
    ;;set canopy-radius sqrt(height * 3.585 / pi)
    set canopy-radius (0.393 * height) / 2
    
    ;;place trees semi-randomly
    loop [
    setxy random-xcor random-ycor
    if count turtles with [stage = "mature"] in-radius 2 < 2 [stop]]
  ]
  
  ;;create sapling maples
  create-maples init-sapling-maple [
    init-maple-values
    set stage "sapling"
    set shape "square"
    set age random 32 + 3
    set height (b1 * site-index ^ b2 * (1 - e ^ (b3 * (age + 1))) ^ (b4 * site-index ^ b5)) * 0.3048
    set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
    set ba pi * (dbh / 2) ^ 2
    ;;set canopy-radius sqrt(height * 3.585 / pi)
    set canopy-radius (0.393 * height) / 2
    setxy random-xcor random-ycor
  ]
  
  ;;create mature poplars
  create-poplars init-mature-poplar [
    init-poplar-values
    set stage "mature"
    set age random-poisson stand-age
    set shape "plant"
    set size 2
    ;;set size based on height and site index (equation is the same, parameters b1-b5 differ)
    set height (b1 * site-index ^ b2 * (1 - e ^ (b3 * (age + 1))) ^ (b4 * site-index ^ b5)) * 0.3048
    
    ;;placeholder!!
    set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
    
    set ba pi * (dbh / 2) ^ 2
    ;;set canopy-radius sqrt(height * 3.585 / pi)
    ;;placeholder!!
    set canopy-radius (0.393 * height) / 2
    
    ;;place trees semi-randomly
    loop [
    setxy random-xcor random-ycor
    if count turtles with [stage = "mature"] in-radius 2 < 2 [stop]]
  ]
  
  ;;create sapling poplars
  create-poplars init-sapling-poplar [
    init-poplar-values
    set stage "sapling"
    set shape "square"
    set color violet
    set age random 32 + 3
    set height (b1 * site-index ^ b2 * (1 - e ^ (b3 * (age + 1))) ^ (b4 * site-index ^ b5)) * 0.3048
    
    ;;placeholder!
    set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
    
    set ba pi * (dbh / 2) ^ 2
    
    ;;set canopy-radius sqrt(height * 3.585 / pi)
    ;;placeholder!!
    set canopy-radius (0.393 * height) / 2
    setxy random-xcor random-ycor
  ]
 
 
 
 
 
  ;;draw canopies of mature and sapling trees
  ask turtles with [stage = "mature" or stage = "sapling"] [draw-canopy]
  ;;color patches accordingly
  ask patches with [midstory-cover > 0] [set pcolor green]
  ask patches with [midstory-cover > 0.3] [set pcolor lime]
  ask patches with [canopy-cover > 0.3] [set pcolor yellow]
  ask patches with [canopy-cover > 0.7] [set pcolor orange]
  ask patches with [canopy-cover > 0.9] [set pcolor red]
  
  set tree-dens count turtles with [dbh >= 0.025]
  set basal-area sum [ba] of turtles with [dbh >= 0.025]
  set prop-oak (sum [ba] of turtles with [dbh >= 0.025 and breed = oaks] / basal-area)
  set prop-tol (sum [ba] of turtles with [dbh >= 0.025 and breed = maples] / basal-area)
  set prop-intol (sum [ba] of turtles with [dbh >= 0.025 and breed = poplars] / basal-area)
  setup-plots
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GO FUNCTION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
  ;;advance year and reset patches to brown
  set year year + 1
  
  let current-age max [age] of oaks
  
  set Hs 1.80408 * (site-index-oak * 0.3048) ^ (0.932097) * (1 - exp(-0.0240308 * current-age)) ^ (2.55529 * (site-index-oak * 0.3048) ^ (-0.280445))
  set Ds 6.40146 * (site-index-oak * 0.3048) ^ (0.631893) * (1 - exp(-0.0227614 * current-age)) ^ (1.21892)
  set J ln((1.12 * Hs - Hs) / (1.12 * Hs - 1.37)) / Ds
  set G 1.12 * Hs - 1.37
  
  ask patches [set midstory-cover 0]
  ask patches [set canopy-cover 0]
  ask patches [set pcolor brown]
  
  ;;start with mature and sapling trees
  ask turtles with [stage = "mature" or stage = "sapling"][
    ;;set crown class (dominant = dom/codominant; non-dominant = intermediate/suppressed)
    check-dominance
    ;;see how much light is available
    calc-available-light
    ;;check if the tree survives the period
    check-survival
    ;;if it does, grow
    grow
    ;;draw the tree canopy (which will effect other trees)
    draw-canopy
  ]
  
  ;;ask mature trees to produce seeds or sapling trees
  ask oaks with [stage = "mature"] [produce-mast]
  ask turtles with [breed = maples or breed = poplars and dbh >= 0.2] [establish-trees]
  
  ;;move fallen acorns and germinate into seedlings
  ask acorns [
    disperse-mast
    germinate-mast
  ]
  
  ;;process seedlings
  ask turtles with [stage = "seedling"][
    ;;check how much light is available
    calc-available-light
    ;;check survival and growth accordingly
    check-survival
    grow
  ]
  if full-harvest = TRUE [set harvestsite 6]
   ;;harvest cycle (if enabled) - occurs last, assumed to be fall/winter
   if year = harvestyear [
     ;;code for shelterwood only - changing index values
     ifelse harvesttype = "shelterwood" [
       conduct-harvest
       set phase phase + 1
       if phase = 4 [
         set harvestsite harvestsite + 1
         if harvestsite = 5 [set harvestsite 1]
         set phase 1]
     ]
     [conduct-harvest
     ;;setup next harvest time
     set harvestyear harvestyear + rotation-length] 
     ]
  
  ;;re-color patches according to available light
  ask patches with [midstory-cover > 0] [set pcolor green]
  ask patches with [midstory-cover > 0.3] [set pcolor lime]
  ask patches with [canopy-cover > 0.3] [set pcolor yellow]
  ask patches with [canopy-cover > 0.7] [set pcolor orange]
  ask patches with [canopy-cover > 0.9] [set pcolor red]

  set tree-dens count turtles with [dbh >= 0.025]
  set basal-area sum [ba] of turtles with [dbh >= 0.025]
  set prop-oak (sum [ba] of turtles with [dbh >= 0.025 and breed = oaks] / basal-area)
  set prop-tol (sum [ba] of turtles with [dbh >= 0.025 and breed = maples] / basal-area)
  set prop-intol (sum [ba] of turtles with [dbh >= 0.025 and breed = poplars] / basal-area)
  
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to grow
  ;;sapling and mature classes
  if height >= 1.5 and height < maxht [
    ;;baseline height growth
    ;;apparent age is how old the tree should be based on its current height
    let apparent-age round((ln(1 - ((height * 3.28084) * site-index ^ (-1 * b2) / b1) ^ (site-index ^ (-1 * b5) / b4))) / b3)
    ;;max growth possible for a tree of that "apparent age"  
    let raw-ht-growth ((b1 * site-index ^ b2 * (1 - e ^ (b3 * (apparent-age + 1))) ^ (b4 * site-index ^ b5)) - 
      (b1 * site-index ^ b2 * (1 - e ^ (b3 * (apparent-age))) ^ (b4 * site-index ^ b5))) * 0.3048
    
    ;;adjusted height growth based on environment
    ;;only grow if > than light cutoff
    if light >= light-cutoff [ 
      ;;scale growth based on avaiable light (light ranges from 0 to 1)
      set height height + raw-ht-growth * light
      if breed = oaks [
      ;;set dbh Johnson 2002 curve
      if (1 - ((height - 1.37) / G)) > 0 [
      set dbh (ln(1 - ((height - 1.37) / G)) / J) * 0.01
      set ba pi * (dbh / 2) ^ 2]
      set canopy-radius (0.376 * height) / 2]
      if breed = maples [
        if height > 1.3 [set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
          set ba pi * (dbh / 2) ^ 2]
      set canopy-radius (0.393 * height) / 2]
      if breed = poplars [
        ;;placeholder!!!!!!
        if height > 1.3 [set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
          set ba pi * (dbh / 2) ^ 2]
        set canopy-radius (0.393 * height) / 2
      ]
      
    ]
    ;;convert saplings with dbh >0.1 to mature trees
    if stage = "sapling" and dbh >= 0.1 [
      set stage "mature" set size 2 
      if breed = oaks [set shape "tree" set acorn-mean random-exponential 10]
      if breed = maples [set shape "leaf"]
      if breed = poplars [set shape "plant"]
      ]   
  ]
  ;;seedling class
  if height < 1.5 [ 
    ;;baseline height growth
    let raw-ht-growth random-normal seedling-growth 0.09    
    set height height + raw-ht-growth   
    ;;convert to sapling class if height >1.5m
    if height >= 1.5 [set stage "sapling" set shape "square" set hidden? FALSE init-oak-values]
  ]    
end



to draw-canopy
  if stage = "mature" [
    ;;canopy leaf density for this species
    let temp canopy-density
    ;;add this tree's canopy to the canopy density w/in its canopy radius
    ask patches in-radius canopy-radius [
      set canopy-cover (canopy-cover + (1 - canopy-cover) * temp)
    ]]
  ;;same, but for saplings in midstory
  if stage = "sapling" [
    let temp midstory-density
    ask patches in-radius canopy-radius [
      set midstory-cover (midstory-cover + (1 - midstory-cover) * temp)
    ]]
end



;;simple function to set a bunch of variables quickly
to init-oak-values
   set color black
   set seedling-growth oak-seedling-growth
   ;;placeholders for canopy density values (need to be calibrated)
   set canopy-density 0.5
   set midstory-density 0.3
   set light-cutoff 0.49
   set site-index site-index-oak
   ;;growth parameters ( 
   set b1 4.5598
   set b2 0.8136
   set b3 -0.0132
   set b4 2.2410
   set b5 -0.1880
   set maxage random 100 + 250
   set maxht G
end

;;simple function to set a bunch of variables quickly
to init-maple-values
  set color blue
  set seedling-growth 0.3
  set light-cutoff 0.05
  set canopy-density 0.7
  set midstory-density 0.5
  set site-index site-index-maple
  set b1 6.1308
  set b2 0.6904
  set b3 -0.0195
  set b4 10.1563
  set b5 -0.5330
  set maxage random 100 + 250
  set maxht 34
end

to init-poplar-values
  set color violet
  ;;placeholder
  set seedling-growth 0.3
  set light-cutoff 0.65
  set canopy-density 0.5
  set midstory-density 0.3
  set site-index site-index-maple
  ;;Carmean 1989
  set b1 1.2941
  set b2 0.9892
  set b3 -0.0315
  ;;error in Carmean 1989 for this!!!
  set b4 1.0471
  set b5 -0.0368
  set maxage random 150 + 100
  set maxht 40
end

to check-dominance
  ;;for saplings
  if stage = "sapling" [
  ;;set to dominant if it's the tallest sapling w/in its canopy radius
  if count turtles with [breed != acorns and stage = "sapling"] in-radius 3 > 0 [
  ifelse height >= max [height] of turtles with [breed != acorns and stage = "sapling"] in-radius 3 [set dominance true]
  [set dominance false]]]
  ;;for mature trees
  if stage = "mature" [
  ;;same as above  
  ifelse height >= max [height] of turtles with [breed != acorns and stage = "mature"] in-radius canopy-radius [set dominance true]
  [set dominance false]]
end



;;calculate light available based on canopy cover (patches) 
to calc-available-light
  ;;all seedlings overtopped by midstory and canopy
  if stage = "seedling" [
    let covertemp (mean [canopy-cover] of patches in-radius 2) 
    let cover2temp (mean [midstory-cover] of patches in-radius 2)
    set light (1 - (covertemp + cover2temp * covertemp))
   ]
  
  ;;dominant saplings overtopped by canopy, non-dominant overtopped by both
  if stage = "sapling" [
    let covertemps (mean [canopy-cover] of patches in-radius 3) 
    let cover2temps (mean [midstory-cover] of patches in-radius 3)
    ifelse dominance = true [
      set light (1 - (covertemps))]
    [set light (1 - (covertemps + cover2temps * covertemps))]
   ]
  
  ;;dominant matures have light = 1, non-dominant matures have light = mean canopy cover in radius
  ;;change color to gray if non-dominant
  if stage = "mature" [
    ifelse dominance = true [ 
      ;set light (1 + canopy-density - (mean [canopy-cover] of patches in-radius canopy-radius))
      set light 1.0
      if breed = oaks [set color black]
      if breed = maples [set color blue]
      if breed = poplars [set color violet]
    ]
    ;;[set light (1 + canopy-density - (mean [canopy-cover] of patches in-radius canopy-radius))
    [set light (1 - (mean [canopy-cover] of patches in-radius canopy-radius))
     if breed = oaks [set color gray] 
     if breed = maples [set color 85]
     if breed = poplars [set color 117]
  ]]
end



to check-survival
 ;;advance age
 set age age + 1
 if breed = oaks [
  if stage = "seedling" [
    ;;survival related to available light
   if random-float 1 > light * 0.97 [die]]   
  if stage = "sapling" [
    ifelse sprout? = TRUE [
    if random-float 1 > light * 0.99 [die]]
    [if random-float 1 > light * 0.97 [die]]]
  if stage = "mature" [
    ifelse age > maxage [die][
      ;;from Fan, Kabrick, Shifley 2006 - survival based on canopy class
      ifelse dominance = true [
        ifelse dbh <= 43 [if random-float 1 > .9976 [create-sprout] ]
        [if random-float 1 > .9922 [create-sprout]]]
      ;;[if random-float 1 > 0.9935 [create-sprout]]]]]
      [if random-float 1 > 0.9872 [create-sprout]]]]]
 ;;placeholder for maple survival
 if breed = maples [
   if stage = "sapling" [
     if random-float 1 > .97 [die]
   ]
   if stage = "mature" [
     ;;from Long, Horsle, Hilja 1997
      ifelse dominance = true [
        if random-float 1 > .997 [create-sprout]]
      [if random-float 1 > 0.992 [create-sprout]]]]
 ;;placeholder for maple survival
 if breed = poplars [
   if stage = "sapling" [
     if random-float 1 > .97 [die]
   ]
   if stage = "mature" [
     ;;from Long, Horsle, Hilja 1997
      ifelse dominance = true [
        if random-float 1 > .997 [create-sprout]]
      [if random-float 1 > 0.992 [create-sprout]]]]
end



to produce-mast
  ;;# of acorns produced per meter squared of capy area
  let acorns-produced (light * 3.1415 * canopy-radius * canopy-radius * random-poisson acorn-mean) ;per meter squared
  let tree-radius canopy-radius
  let treexcor xcor
  let treeycor ycor
  hatch-acorns acorns-produced
  ;;drop produced acorns under canopy randomly
   [ set hidden? TRUE
     set color black
     set xcor (treexcor + random tree-radius - random tree-radius - 0.5)
     set ycor (treeycor + random tree-radius - random tree-radius - 0.5)
     ifelse random-float 1 < 0.5 [set weevil TRUE] [set weevil FALSE] ;;check to see if weeviled
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

;;chance of acorn germination
to germinate-mast
  ifelse cached = TRUE [
    set germ germ-prob]
  [ifelse weevil = TRUE [set germ germ-prob * 0.1 * 0.1] 
    [set germ germ-prob * 0.1]]  
  if random-float 1 < germ [
      hatch-oaks 1 [
        set age 1
        set size 1
        init-oak-values
        set stage "seedling"    
        set hidden? TRUE
      ]]
    die
end

to establish-trees
  if breed = maples [
  ;;sapling-stage maples produced randomly from mature maples to save time
  if random-float 1 < maple-establish-prob [
  ;;let seedlings-produced (light * 3.1415 * canopy-radius * canopy-radius *  0.44) ;;per meter squared
  let treexcor xcor
  let treeycor ycor
  ;;hatch a new maple sapling
  hatch-maples 1
  [
    init-maple-values
    set size 1
    set age 4
    set stage "sapling"
    set shape "square"
    set height 1.5
    set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
    ;;set canopy-radius sqrt(height * 3.585 / pi)
    set canopy-radius (0.393 * height) / 2
    right random 360
    forward random-float 28
  ]]]
  if breed = poplars [
    if random-float 1 < poplar-establish-prob [
  ;;let seedlings-produced (light * 3.1415 * canopy-radius * canopy-radius *  0.44) ;;per meter squared
  let treexcor xcor
  let treeycor ycor
  ;;hatch a new maple sapling
  hatch-poplars 1
  [
    init-poplar-values
    set size 1
    set age 4
    set stage "sapling"
    set shape "square"
    set color violet
    set height 1.5
    ;;placeholders!!
    set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
    ;;set canopy-radius sqrt(height * 3.585 / pi)
    set canopy-radius (0.393 * height) / 2
    right random 360
    forward random-float 28
  ]]]
end




to create-sprout
  ;;From Dey 2002
  if breed = oaks [
    let prob (1 + exp(-1 * (5.9991 - 0.2413 * (dbh * 39.3701) - 0.0475 * age))) ^ (-1)
    ifelse random-float 1 < prob [
      set stage "sapling" set shape "square" set color black set sprout? TRUE
      set height 5.13286 - 0.07647 * (dbh * 39.3701) - 0.015 * age
      set dbh (ln(1 - ((height - 1.37) / G)) / J) * 0.01
      set canopy-radius (0.376 * height) / 2
      set ba pi * (dbh / 2) ^ 2
      set age 1
      set maxage random 100 + 250
      set maxht G
    ]
    [die]
  ]
  if breed = maples [
    ;;Based on Powell 1983
    let prob -0.314 * ln(dbh) + 0.0877
    if prob > 1 [set prob 1]
    if dbh > 0.8 [set prob 0]
    ifelse random-float 1 < prob [
      set stage "sapling" set shape "square" set color blue set sprout? TRUE
      set height 5.003 * dbh * dbh - 3.8167 * dbh + 1.274
      set age 1
      set dbh 0
      if height > 1.3 [      
      set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
      set ba pi * (dbh / 2) ^ 2]
      set canopy-radius (0.393 * height) / 2
    ]
    [die]
  ]
  if breed = poplars [
    ;;based on Wendel 1975 plus some guesses
    let prob 0.97
    if dbh > 0.8 [set prob 0]
    ifelse random-float 1 < prob [
      set stage "sapling" set shape "square" set color violet set sprout? TRUE
      set height random-normal 1.3 0.3
      set dbh 0
      set age 1
      if height > 1.3 [      
      set dbh ((-15.4112 / ln((height - 1.3) / 36.8618)) - 3.8011) * 0.01
      set ba pi * (dbh / 2) ^ 2]
      set canopy-radius (0.393 * height) / 2
    ]
    [die]]
end

to conduct-harvest
  if harvest = true and harvesttype = "clearcut" [
    ;;cut all mature trees in one quadrant
  if harvestsite = 1 [
    ask turtles with [(dbh >= 0.0254) and (pxcor <= 0) and (pycor > 0)] [create-sprout]]
  if harvestsite = 2 [
    ask turtles with [(dbh >= 0.0254) and (pxcor > 0) and (pycor >= 0)] [create-sprout]]
  if harvestsite = 3 [
    ask turtles with [(dbh >= 0.0254) and (pxcor >= 0) and (pycor < 0)] [create-sprout]]
  if harvestsite = 4 [
    ask turtles with [(dbh >= 0.0254) and (pxcor < 0) and (pycor <= 0)] [create-sprout]]
  set harvestsite harvestsite + 1
  if harvestsite = 5 [set harvestsite 1]
  if harvestsite > 5 [
    ask turtles with [(dbh >= 0.0254)] [create-sprout]]
  ]
  
  
  if harvest = true and harvesttype = "group" [
    ;;cut same number of trees, but in small clusters
    ask max-n-of 8 turtles [dbh] [
    ask turtles with [stage = "mature"] in-radius 10 [
      create-sprout
    ]]
  ]
  
  if harvest = true and harvesttype = "single-tree" [
    ;;same number of trees, but individual trees scattered around the area
    ask max-n-of 20 turtles [dbh] [
      create-sprout
    ]]
  
  
  if harvest = true and harvesttype = "shelterwood" [
    ;;similar to clearcut but harvesting done in phases targetting oak (in one quadrant)
  if harvestsite = 1 [
    if phase = 1 [
      ask maples with [(height >= 1.5) and (pxcor <= 0) and (pycor > 0)] [create-sprout]
      set harvestyear harvestyear + 5 ]
    if phase = 2 [
      ask oaks with [(stage = "mature") and (dominance = FALSE) and (pxcor <= 0) and (pycor > 0)] [create-sprout]
      set harvestyear harvestyear + 10 ]
    if phase = 3 [
      ask oaks with [(stage = "mature") and (pxcor <= 0) and (pycor > 0)] [create-sprout]
      set harvestyear harvestyear + 10 
      ]
    ]
  if harvestsite = 2 [
    if phase = 1 [
      ask maples with [(height >= 1.5) and (pxcor > 0) and (pycor >= 0)] [create-sprout]
      set harvestyear harvestyear + 5 ]
    if phase = 2 [
      ask oaks with [(stage = "mature") and (dominance = FALSE) and (pxcor > 0) and (pycor >= 0)] [create-sprout]
      set harvestyear harvestyear + 10 ]
    if phase = 3 [
      ask oaks with [(stage = "mature") and (pxcor > 0) and (pycor >= 0)] [create-sprout]
      set harvestyear harvestyear + 10
      ]
    ]
  if harvestsite = 3 [
    if phase = 1 [
      ask maples with [(height >= 1.5) and (pxcor >= 0) and (pycor < 0)] [create-sprout]
      set harvestyear harvestyear + 5 ]
    if phase = 2 [
      ask oaks with [(stage = "mature") and (dominance = FALSE) and (pxcor >= 0) and (pycor < 0)] [create-sprout]
      set harvestyear harvestyear + 10 ]
    if phase = 3 [
      ask oaks with [(stage = "mature") and (age > 24) and (pxcor >= 0) and (pycor < 0)] [create-sprout]
      set harvestyear harvestyear + 10
      ]
  ]
  if harvestsite = 4 [  
    if phase = 1 [
      ;harvest other junk trees
      ask maples with [(height >= 1.5) and (pxcor < 0) and (pycor <= 0)] [create-sprout]
      set harvestyear harvestyear + 5 ]
    if phase = 2 [
      ask oaks with [(stage = "mature") and (dominance = FALSE) and (pxcor < 0) and (pycor <= 0)] [create-sprout]
      set harvestyear harvestyear + 10 ]
    if phase = 3 [
      ask oaks with [(stage = "mature") and (age > 24) and (pxcor < 0) and (pycor <= 0)] [create-sprout]
      set harvestyear harvestyear + 10
      ]
  ]
    if harvestsite > 5 [  
    if phase = 1 [
      ;harvest other junk trees
      ask maples with [(height >= 1.5) and (pxcor < 0) and (pycor <= 0)] [create-sprout]
      set harvestyear harvestyear + 5 ]
    if phase = 2 [
      ask oaks with [(stage = "mature") and (dominance = FALSE) and (pxcor < 0) and (pycor <= 0)] [create-sprout]
      set harvestyear harvestyear + 10 ]
    if phase = 3 [
      ask oaks with [(stage = "mature") and (age > 24) and (pxcor < 0) and (pycor <= 0)] [create-sprout]
      set harvestyear harvestyear + 85
      ]
  ]]
end

;;to-report basal-area
;;  let total sum [ba] of turtles with [dbh >= 0.025]
;;  report total
;;end














  
@#$#@#$#@
GRAPHICS-WINDOW
210
42
1028
881
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

SLIDER
20
689
192
722
rotation-length
rotation-length
0
100
93
1
1
years
HORIZONTAL

SLIDER
16
45
188
78
init-mature-oak
init-mature-oak
0
300
199
1
1
oaks
HORIZONTAL

SLIDER
16
10
188
43
stand-age
stand-age
0
200
80
1
1
years
HORIZONTAL

BUTTON
19
416
82
449
setup
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
99
415
162
451
go
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

MONITOR
19
503
94
548
BA (m2/ha)
basal-area
2
1
11

SLIDER
17
840
192
873
oak-seedling-growth
oak-seedling-growth
0
0.5
0.49
0.01
1
m
HORIZONTAL

SLIDER
16
82
188
115
init-sapling-oak
init-sapling-oak
0
2000
561
1
1
oaks
HORIZONTAL

SLIDER
16
120
188
153
init-seedling-oak
init-seedling-oak
0
5000
2006
1
1
oaks
HORIZONTAL

SLIDER
17
881
189
914
germ-prob
germ-prob
0
1
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
15
279
187
312
site-index-oak
site-index-oak
30
80
60
1
1
feet
HORIZONTAL

SLIDER
15
157
203
190
init-mature-maple
init-mature-maple
0
300
42
1
1
maples
HORIZONTAL

SLIDER
15
198
202
231
init-sapling-maple
init-sapling-maple
0
500
52
1
1
maples
HORIZONTAL

SLIDER
20
761
204
794
maple-establish-prob
maple-establish-prob
0
1
0.15
0.01
1
NIL
HORIZONTAL

CHOOSER
21
640
159
685
harvesttype
harvesttype
"clearcut" "shelterwood" "single-tree" "group"
2

SWITCH
21
604
124
637
harvest
harvest
1
1
-1000

SLIDER
15
239
187
272
site-index-maple
site-index-maple
30
80
60
1
1
feet
HORIZONTAL

MONITOR
18
454
85
499
mean dbh
sqrt(basal-area / (0.0000785 * count turtles with [dbh > 0.025]))
2
1
11

MONITOR
89
454
178
499
mean dbh (in)
2 * (sqrt(mean [ba] of turtles with [dbh >= 0.025] / pi)) * 39.37
2
1
11

MONITOR
98
503
183
548
ba (ft2/acre)
basal-area * 4.356
2
1
11

MONITOR
19
552
111
597
trees per acre
count turtles with [dbh >= 0.025] * 0.40477
2
1
11

MONITOR
117
551
198
596
trees per ha
count turtles with [dbh >= 0.025]
2
1
11

SLIDER
17
802
189
835
poplar-establish-prob
poplar-establish-prob
0
1
0.39
0.01
1
NIL
HORIZONTAL

SLIDER
15
348
210
381
init-mature-poplar
init-mature-poplar
0
300
25
1
1
poplars
HORIZONTAL

SLIDER
15
314
207
347
init-sapling-poplar
init-sapling-poplar
0
1000
50
1
1
poplars
HORIZONTAL

SWITCH
94
604
213
637
full-harvest
full-harvest
1
1
-1000

PLOT
1050
106
1250
256
Basal Area
Years
M2/ha
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
1070
309
1270
459
Density
Years
Trees / ha
0.0
10.0
0.0
10.0
true
false
"set-plot-y-range (precision (tree-dens - 100) 0) (precision (tree-dens + 100) 0)\nplot-pen-up\nplotxy 0 tree-dens\nplot-pen-down" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [dbh >= 0.025]"

PLOT
1046
491
1246
641
Prop Oak
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot prop-oak"
"pen-1" 1.0 0 -13345367 true "" "plot prop-tol"
"pen-2" 1.0 0 -6917194 true "" "plot prop-intol"

@#$#@#$#@
## WHAT IS IT?

This is a forest model, simulating the growth, survival, and reproduction of three tree species (oak, poplar, and maple) over time and under different timber harvesting management scenarios. The agents are individual trees in three categories (seedling, sapling and mature) and the patches in the environment differ in how much light is available (which in turn depends on the canopies of nearby trees).

## HOW IT WORKS

Individual trees survive, grow, and reproduce based on the light available to them in the patches around them. Each species follows different rules for how light affects these processes; for shade intolerant species like oak and poplar, more light is required for growth than a shade tolerant like maple. Different harvesting techniques can change light availability for young trees (e.g. clearcut vs. removal of single trees) and so the final stand composition after many years under different management can be expected to also be different.

Patch colors represent the canopy density (red = more dense, yellow = less dense overstory; dark green = more dense, light green = less dense midstory; brown = no cover). Mature oaks are represented by a "tree" shape and black color, mature maples by a "maple leaf" and blue color and mature poplars by a "plant" and purple color. All saplings are square shaped but the colors differ as above by species. Stump-origin saplings are larger squares that seed-origin saplings. Acorns and seedlings are set to "hidden" by default to make things more simple.

## HOW TO USE IT

The parameters above the "go" and "setup" buttons are used to set the intial conditions of the stand. Primarily this means the number of initial individuals for each species and size category, but also the site index for oaks and maples (that is, the site quality; the higher the value the faster the tree will grow).

Immeadiatly below the "go" and "setup" buttons are a series of reporters for relevant stand variables including total stand basal area, tree density, and mean diameter. These are common variables in the forestry literature and are presented in both metric and Imperial units since the units used in forestry charts are often one or the other.

The next group of sliders and buttons relate to the harvest regime implemented. Harvest can be turned "on" or "off". "Full-harvest" refers to the area harvested - if "on", the entire stand will receive the harvest; if "off", the stand will be divided into four individual sub-stands that receive harvests sequentially (useful for looking at forest successional stages). The type of harvest can be selected with "harvesttype" and includes clearcut, shelterwood, group selection, and single-tree selection harvest types. The rotation length is the time between harvests in a stand.

The final group of sliders are various tuning parameters, including the individual recruitment probability for maples and poplars (the probability a mature individual will produce a single new sapling in a given year, in lieu of a more complex method), the growth rate of oak seedlings, and the germination probability of oak acorns. These can be manipulated to fine-tune the final stand structure.

## THINGS TO TRY

Try the different harvest technique settings, let the model run for 200-300 years (time steps) and see how forest species composition has changed from the intial values. Typically, the uneven-aged methods (group and single-tree selection) will favor maple, while the clearcut setting will favor oak.

## EXTENDING THE MODEL

Right now, the numerous parameters associated with acorn survival and dispersal are essentially fixed. In future versions of the model they will be dynamic in order to determine the role that seed predators/dispersal agents play on generating new oak seedlings and implications for final stand structure.
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
<experiments>
  <experiment name="harvesting" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10"/>
    <metric>sum [ba] of oaks</metric>
    <metric>sum [ba] of maples</metric>
    <metric>sum [ba] of poplars</metric>
    <enumeratedValueSet variable="oak-seedling-growth">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poplar-establish-prob">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maple-establish-prob">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="site-index-maple">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stand-age">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-sapling-maple">
      <value value="52"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="full-harvest">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-sapling-poplar">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="site-index-oak">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-mature-maple">
      <value value="42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-mature-oak">
      <value value="199"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-sapling-oak">
      <value value="561"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvesttype">
      <value value="&quot;clearcut&quot;"/>
      <value value="&quot;shelterwood&quot;"/>
      <value value="&quot;group&quot;"/>
      <value value="&quot;single-tree&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rotation-length">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-seedling-oak">
      <value value="2006"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-mature-poplar">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="germ-prob">
      <value value="0.9"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
