;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        SOEL: Simulating Early Oak Life history            ;;
;;                An Individual-based Model                  ;;
;;        See S1 SOEL Description for more details           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;NetLogo extensions imported
extensions [profiler matrix]

;Name variables that are constant across the simulation (and are not set via slider/chooser)
globals [

  ;Setup Parameters
  xcutoff ycutoff
  sitequal-boak sitequal-woak sitequal-maple sitequal-poplar
  harvest-year shelter-phase

  ;Input Parameters
  wo-mast-list bo-mast-list mast-year-index
  mast-mean-bo mast-mean-wo mast-mean-comb
  core-acorn-params-bo buffer-acorn-params-bo
  core-acorn-params-wo buffer-acorn-params-wo
  core-weev-prob-bo buffer-weev-prob-bo
  core-weev-prob-wo buffer-weev-prob-wo
  surv-params growth-params br-year-ef
  sens-params
  prioryears mastsd

  ;Overstory Output Parameters
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

  ;Regeneration Output Parameters
  acorn-count total-acorns total-seedlings new-seedlings pct-germ
  regen-dens regen-stump-dens
  seedlings-class1 seedlings-class2 seedlings-class3 seedlings-class123 seedlings-class4 acorns-pertree
  seedbo-class123 seedwo-class123 seedbo-class4 seedwo-class4
  bo-acorn-count wo-acorn-count
  new-bo-seedlings new-wo-seedlings
  total-bo-acorns total-wo-acorns
  pct-bo-germ pct-wo-germ

  ]

;Set up descriptive variables for agents and patches in the simulation

turtles-own [in-core] ;is the agent in the core area (i.e., not in the buffers?)

;Set up "breeds", i.e., different tree species and define the descriptive variables each individual tree has (oaks have more than the others)
breed [oaks oak]
oaks-own [species light age dbh height canopy-radius canopy-density actual-growth ba acorn-mean seedling sprout? fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality density browsed indeffect
  ]

breed [maples maple]
maples-own [light age dbh height canopy-radius canopy-density actual-growth ba seedling sprout? fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality density
  ]

breed [poplars poplar]
poplars-own [light age dbh height canopy-radius canopy-density actual-growth ba seedling sprout? fAL
  Dmax Hmax Amax b2 b3 C G light-tol intrinsic-mortality min-increment growth-mortality density
  ]

breed [acorns acorn] ;acorns are their own breed and have their own variables
acorns-own [species weevil cached germ disp-dist]

patches-own [stem-dens shade in-layer1 in-layer2] ;patches keep track of the stem density and canopy cover


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Setup function: initializes the simulation
to setup

  ;Clean up old data if it is present
  clear-all
  reset-ticks

  ;Resize the simulation as requested by the user (variable 'adjust' is used later to calculate area-specific variables)
  ;World is made up of 1x1 m patches
  set xcutoff (x-core / 2)
  set ycutoff (y-core / 2)
  let adjust (x-core + buffer * 2) * (y-core + buffer * 2) / 10000
  resize-world (-1 * (xcutoff + buffer)) (xcutoff + buffer) (-1 * (ycutoff + buffer)) (ycutoff + buffer)

  ;Take specified site condition variables and calculate site quality for each species
  calc-site-quality

  ;Vectors below represent 1/meanAcorn values for the HEE, in order by year
  set bo-mast-list [0.0827 0.0489 2.975 0.8809 0.0757 0.1153 0.0661 0.1415 0.05667]
  set wo-mast-list [0.1355 0.0665 0.1092 2.720 0.0367 0.136 0.1070 0.238 0.0406]
  set mast-year-index random 9  ;If mast-scenario is 'hee', pick a random year to start within the yearly progressions above

  ;HEE-based initial forest variables
  ifelse HEE-mean = TRUE [
    init-stand adjust TRUE 89 11 9 95 499 163] [ ;Initial stand values based on Saunders and Arseneault 2013
    init-stand adjust FALSE mature-oak mature-maple mature-poplar sapling-oak sapling-maple sapling-poplar] ;User defined

  ask patches [color-patches] ;Color patches based on canopy cover

  calc-global-vars ;Calculate all global variables at initialization

  set harvest-year burnin ;Set-up when harvest will occur (after burn-in)

  set shelter-phase 1 ;Set shelter-phase to 1 to initialize the shelterwood harvest (if necessary)

  setup-plots ;Initialize plots

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function that is called at each time step ('tick') of the simulation
to go

  calc-light ;Calculate light available to each tree

  set-scenario ;Set up early oak life history parameter values for this time step

  set acorn-count 0 set bo-acorn-count 0 set wo-acorn-count 0 ;Reset acorn counters to 0 (no seed bank)

  ;Iterate over all non-seedling trees
  ask turtles with [seedling = FALSE] [
    grow ;Grow based on environment
    check-survival ;Determine survival based on growth
    if breed = oaks and dbh >= 0.20 and seedlings != "none" [produce-acorns] ;Oaks produce acorns; DBH cutoff based on Downs and McQuilkin 1944
  ]

  ;Iterate over all acorns produced this time step
  ask acorns [
   disperse-mast ;Dispersers move and cache acorns individually
   germinate-mast ;Check if the acorn germinates into a seedling based on environment
  ]

  ;If individual seedlings tracked: iterate over all seedlings
  if seedlings = "hee" [
    ask oaks with [seedling = TRUE] [
      grow-seedling ;Grow based on environment
      check-seedling-survival ;Determine survival based on environment
    ]
  ]

  ;Iterate over all patches
  ask patches [
    regenerate ;Check if a new maple or poplar appears in the patch based on the environment (sensu JABOWA)
    color-patches ;Re-color patches based on new forest structure
  ]

  conduct-harvest ;Check if a harvest should occur and if so conduct it

  calc-global-vars ;Calculate global variables at the end of the time step and report them as necessary

  tick ;Move to next year
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                    Basic JABOWA Procedures                      ;;;;;;;;
;;;;;;;;                 "Contextual Forest" Submodel                    ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;See S1 SOEL Description, section 5 for more details

;Function that generates tree characteristics from dbh; see S1 Section 5.2
to allometerize [dbh-input] ;only input is dbh

  ;Convert dbh to height based on JABOWA, Botkin 1993
  ;b2 = 2*(Hmax - 137) / Dmax and b3 = (Hmax - 137) / Dmax ^2
  set height (137 + b2 * (dbh-input * 100) - b3 * ((dbh-input * 100) ^ 2)) / 100

  ;Convert height to canopy radius based on Kenefic and Nyland 1999 and others
  ;range reported is 0.376 - 0.393; going with .385 for now (matches HEE data)
  set canopy-radius max list 1 (0.385 * height / 2)

  ;Convert dbh to leaf weight with species-specific C conversion factor (Botkin 1993)
  ;Exponent can be anywhere from 1.5 to 3 (Botkin 1993). Value of 2.4 was also tested.
  let SLA (C * (dbh-input * 100) ^ (2))

  ;Calculate proportion of light that is blocked by the canopy of the tree
  let PHI 1  ;Based on Beer-Lambert law where light above canopy (PHI) = 1
  let k light-extinct ;Light extinction coefficient (rate at which light is absorbed)
  ;let k 0.0000756 ;Based on QuabbinPlot206, seems far too small
  ;let k 1 / 6000 ;Based on Botkin 1993, allows too much light under big trees
  ;let k 1 / 3000 ;Happy medium
  set canopy-density min list 0.98 (1 - (PHI * exp(-1 * k * SLA))) ;Calculate absorbance based on leaf weight (max 98%)

  set ba (pi * (dbh / 2) ^ 2) ;Convert dbh to basal area
end


;Function that generates maximum possible diameter growth in a year (JABOWA; Botkin 1993; S1 Section 5.2)
to-report max-growth-increment [dbh-input height-input] ;Takes current dbh and height as input
  let num (G * dbh-input * ((1 - (dbh-input * (137 + (b2 * dbh-input) - (b3 * dbh-input ^ 2))) / (Dmax * Hmax))))
  let denom (274 + (3 * b2 * dbh-input) - (4 * b3 * dbh-input ^ 2))
  report (num / denom)
end


;Calculate effect of light available to a given tree on growth depending on shade tolerance; see S1 Section 5.1, Botkin 1993
to-report light-growth-index [light-input tol-input] ;Takes available light and species tolerance class as input
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


;Calculate effect of site climate (degree-days) on site quality for a given species; S1 Section 5.1, Botkin 1993
to-report degree-days-index [degdays-input degd-min-input degd-max-input]; Takes site degree-days and min/max tolerable for species as input
  let tdegd (4 * (degdays-input - degd-min-input) * (degd-max-input - degdays-input) / ((degd-max-input - degd-min-input) ^ 2))
  report max list 0 tdegd
end


;Calculate effect of distance to water table on site quality for a given species; S1 Section 5.1, Botkin 1993
to-report saturation-index [wt-input wt-dist-min-input]; Takes site distance and species minimum water table distance as input
  let wefi (1 - (wt-dist-min-input / wt-input))
  report max list 0 wefi
end


;Calculate effect of available nitrogen at site on site quality; S1 Section 5.1, Botkin 1993
to-report nitrogen-index [N-input N-tol-input] ;Takes site nitrogen available and nitrogen tolerance class of species as input
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

;Calculate effect of wilt index on site quality for a given species; S1 Section 5.1, Botkin 1993
to-report wilt-index [wilt-input wilt-max]
  ;wilt is difference between potential and actual evapotranspiration divided by potential evapotranspiration
  let wifi (1 - (wilt-input / wilt-max) ^ 2)
  report max list 0 wifi
end


;Calculate diameter growth; maximum growth adjusted by site quality and light available; S1 Section 5.1-5.2, Botkin 1993
to grow

  let max-growth (max-growth-increment (dbh * 100) (height * 100)) ;Calculate max possible growth for tree

  set fAL light-growth-index light light-tol ;Calculate light effect on growth given light at tree's XYZ position

  ;Grab appropriate site quality for species
  let sitequal 1
  ifelse breed = oaks [ifelse species = "WO" [set sitequal sitequal-woak] [set sitequal sitequal-boak]]
  [ifelse breed = maples [set sitequal sitequal-maple]
    [set sitequal sitequal-poplar]]

  set actual-growth (max-growth * fAL * sitequal / max list 1 density) ;Adjust max growth based on light, site quality, and nearby stem density (S1 Figure 4)
  set dbh (dbh * 100 + actual-growth) / 100 ;Calculate new dbh after growth
  allometerize dbh ;Generate other tree variables from new dbh based on allometric equations

  ;If tree is sapling and becomes large enough to be mature tree (>=10 cm dbh), change icon appropriately
  if dbh >= 0.1 and shape = "square" [
    if breed = oaks [set size 2 set shape "tree"]
    if breed = maples [set size 2 set shape "tree"]
    if breed = poplars [set size 2 set shape "tree"]
  ]

end


;Function to check if a given tree survives in a time step (check occurs after growth is calculated); S1 Section 5.3, Botkin 1993
to check-survival

  ;If above 5 cm dbh, trees don't immeadiatly die - first the model checks to see if they resprout
  ifelse dbh >= 0.05 [
    if random-float 1 < intrinsic-mortality [create-sprout] ;Intrinsic mortality is 4/max age
    if actual-growth < min-increment and random-float 1 < growth-mortality [create-sprout] ;Additional mortality chance (37%) if below minimum rate of growth

  ] [ ;If below 5 cm, no resprouting occurs
    if random-float 1 < intrinsic-mortality [die] ;Removed from simulation
    if actual-growth < min-increment and random-float 1 < growth-mortality [die]
  ]

  set age age + 1 ;Increase age of surviving trees by 1 year

end


;Trees above 5 cm that die have a chance to sprout based on this function; S1 Section 5.4
to create-sprout

  ifelse sprouting = FALSE [die][ ;If sprouting is turned off, trees just die

  let prob 0 ;Initialize probability of sprouting at 0

  ;Calculate probability of sprouting; only occurs for trees with dbh between 5 and 80 cm
  if dbh < 0.8 and dbh > 0.05 [

    ;Oak resprouting probability based on dbh and age, regression model from Weigel and Peng 2002
    if breed = oaks [
      if species = "WO" [
        set prob (1 + exp(-1 * (-53.6225 - 1.7003 * ln(dbh * 100) - 0.00534 * age * ln(dbh * 100) + 25.7155 * ln(22) - 0.2913 * 22 * ln(22)))) ^ (-1)]
      if species = "BO" [
        set prob (1 + exp(-1 * (-8.1468 - 0.00055 * age * (dbh * 100) + 3.1678 * ln(22)))) ^ (-1)]
    ]

    ;Other species sprout probability
    if breed = maples [set prob -0.314 * ln(dbh) + 0.0877] ;Based on Powell 1983
    if breed = poplars [set prob 1.1832 - 1.3638 * dbh] ;based on Wendel 1975, True 1953, Beck & Della-Bianca 1981
    if prob > 1 [set prob 1] ;Quality control check for small values above 1

  ]

  ;Check if tree sprouts based on sprout probability
  ifelse random-float 1 < prob [ ;If the tree sprouts:
    set sprout? TRUE ;Identify as sprout
    set dbh 0.01; Re-set dbh to 0.01
    init-params; Re-set initial characteristics
    set size 2 ;Larger sized icon to visually show it's a sprout
  ]
  [die] ;If not, die
  ]

end


;Function to allow reproduction of non-oaks in the model by spawning new saplings in 1x1m patches; S1 Section 5.4
;Regeneration probability is based on Botkin 1993 but adjusted for smaller cells in SOEL (1m2) relative to JABOWA (100m2)
to regenerate

  if count turtles-here with [seedling = FALSE] > 0 [stop] ;Can't spawn new saplings if oak seedlings are already present in 1x1 m patch

  ;Set parameters for reproduction in the patch; Botkin 1993, S1 Section 5.4
  let AL light-growth-index ((1 - shade)) "high" ;Light growth index for maples (high shade tolerance)
  let Al-pop light-growth-index ((1 - shade)) "low" ;Light growth index for poplars (low shade tolerance)
  let max-maple-saplings 3 let max-poplar-saplings 10 ;Max maples or poplars that could spawn into a given 100m2 area in one time step

  ;Check if a maple spawns in given cell
  ;Probability based on max possible saplings, light available, and site quality; adjusted for smaller cell size (multiply  by 0.01)
  if random-float 1 < (0.01 * max-maple-saplings * AL * sitequal-maple)[
    sprout-maples 1 [ ;If check is passed, generate a single new maple sapling in cell
      check-in-core ;Check if new sapling is in core area
      set dbh random-float 0.00468 ;Set new dbh
      init-params ;Initialize other variables for new sapling
    ]]

  ;Check if poplar spawns in given cell
  ;Probability similar to above, but shade has to be less than 1% for shade intolerant poplar to spawn
  if shade <= 0.01 and random-float 1 < (0.01 * max-poplar-saplings * AL-pop * sitequal-poplar)[
    sprout-poplars 1 [
      check-in-core
      set dbh random-float 0.0039
      init-params
    ]]

  ;If the model is set to seedlings = "none" instead of "hee", the early oak life history processes will be skipped
  ;Sapling oaks will be generated the same way tulip poplar and maple are above instead
  if seedlings = "none" [
    let Al-oak light-growth-index ((1 - shade) / max list 1 stem-dens ) "intermediate"
    let max-oak-saplings 10
    let sitequal-oak 1
    ifelse random-float 1 < 0.5 [set sitequal-oak sitequal-woak][set sitequal-oak sitequal-boak]

    ;Check if poplar spawns in given cell
    ;Probability similar to above, but shade has to be less than 50% for oak to spawn
    if shade < 0.50 and random-float 1 < (0.01 * max-oak-saplings * AL-oak * sitequal-oak)[
    sprout-oaks 1 [
      check-in-core
      set dbh random-float 0.0041
      ifelse random-float 1 > 0.5 [set species "WO"][set species "BO"] ;Oak species chosen randomly
      init-params
    ]]

  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                Early Oak Lifecycle Procedures                   ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Functions related to the Early Oak Life History submodel described in S1 SOEL Description section 4

;Function that takes the early oak life history scenarios defined by the user and adjusts parameters accordingly
to set-scenario

  ;Set effect of mast-scenario (acorn production)
  ;Values for black and white oak set separately

  ;If "custom", set mast production values (parameter 'meanAcorn') to provided input value
  if mast-scenario = "custom" [
    set mast-mean-wo mast-val
    set mast-mean-bo mast-val
  ]

  ;If "fixedaverage", set meanAcorn to mean production value across all HEE data
  if mast-scenario = "fixedaverage" [
    set mast-mean-wo 11.740
    set mast-mean-bo 11.071
  ]

  ;If "fixedbad" set meanAcorn to mean of bad mast years in HEE data
  if mast-scenario = "fixedbad" [
    set mast-mean-wo 8.579
    set mast-mean-bo 3.749
  ]

  ;If "fixedgood" set meanAcorn to mean of good mast years in HEE data
  if mast-scenario = "fixedgood" [
    set mast-mean-wo 22.346
    set mast-mean-bo 16.584
  ]

  ;If "random" calculate a value for meanAcorn based on an exponential regression model of the HEE data (with random year effect)
  ;See files 'parameter_acorn_production.R' and 'model_acorn_production.R' in S3 Regression Models for more details
  if mast-scenario = "random" [
    set mast-mean-wo min (list exp(2.001 + random-normal 0 1.126) 50)
    set mast-mean-bo min (list exp(2.001 + random-normal 0 1.126) 50)
  ]

  ;If "hee", iterate sequentially through the actual meanAcorn values in the 9 years of HEE data (preserves masting cycles)
  if mast-scenario = "hee" [
    set mast-mean-wo 1 / (item mast-year-index wo-mast-list)
    set mast-mean-bo 1 / (item mast-year-index bo-mast-list)
  ]

  ;If "priordifference", meanAcorn is higher or lower during a 1-2 year period just before a harvest occurs
  ;See SOEL manuscript, "Interaction of Harvest Timing and Acorn Production" experiment for more details
  if mast-scenario = "priordifference" [

    if prioryears = 2 [ ;If the pre-harvest period is 2 years long

      ifelse ticks = (burnin - 2) OR ticks = (burnin - 1) [ ;If within the defined pre-harvest period
        set mast-mean-wo exp(2.001 + mastsd * 1.126) ;meanAcorn is increased or decreased by value 'mastsd' (=number of standard deviations)
        set mast-mean-bo exp(2.001 + mastsd * 1.126)

        ][ ;If not within the pre-harvest period, meanAcorn varies randomly as above under "random"
        set mast-mean-wo min (list exp(2.001 + random-normal 0 1.126) 70)
        set mast-mean-bo min (list exp(2.001 + random-normal 0 1.126) 70)
      ]
    ]

    if prioryears = 1 [ ;Same as above, but pre-harvest period is only 1 year long
      ifelse ticks = (burnin - 1) [
        set mast-mean-wo exp(2.001 + mastsd * 1.126)
        set mast-mean-bo exp(2.001 + mastsd * 1.126)
        ][
        set mast-mean-wo min (list exp(2.001 + random-normal 0 1.126) 70)
        set mast-mean-bo min (list exp(2.001 + random-normal 0 1.126) 70)
      ]
    ]
  ]

  ;Special mast-scenario for calculating sensitivity of model to meanAcorn parameter; a fixed, constant value is provided
  if mast-scenario = "sensitivity" [
    set mast-mean-wo min (list exp(item 0 sens-params) 50)
    set mast-mean-bo min (list exp(item 0 sens-params) 50)
  ]

  ;Models are in rate scale rather than lambda
  set mast-mean-comb 1 / (mean (list mast-mean-wo mast-mean-bo))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Set effect of weevil-scenario (acorn weevil infestation probability, parameter pWeevil)

  ;If "custom", use value provided as input by user
  if weevil-scenario = "custom" [ ]

  ;The following four scenarios make use of a logistic regression model of weevil infestation probability fit using HEE data
  ;The model can be found in file 'parameter_weevil_infestation.R' in S3 Regression Models
  ;Different probabilities are specificied for (1) different species and
  ;(2) whether or not the acorn fell in a "harvest treatment"/core area (i.e., shelterwood/midstory removal) or an unharvested/buffer area
  ;See SOEL manuscript, section "Harvest and Yearly-Varying Impacts on Seed Predation" for more details

  ;If "fixedaverage", use average value across all HEE data (basically, the transformed regression model intercept)
  if weevil-scenario = "fixedaverage" [
    ;Set weevil probability to value returned by gen-weev-prob
    ;gen-weev-prob uses the fitted regression model to return a probability depending on the given scenario
    set buffer-weev-prob-bo (gen-weev-prob -1.08 0 0 0 0)
    set buffer-weev-prob-wo (gen-weev-prob -1.08 0 0 0 1)
    set core-weev-prob-bo buffer-weev-prob-bo
    set core-weev-prob-wo buffer-weev-prob-wo
  ]

  ;If 'yearly-diff', value of pWeevil varies with year (combines random year and mast availability effects) and species only
  if weevil-scenario = "yearly-diff" [
    let randN random-normal 0 1.01
    set buffer-weev-prob-bo (gen-weev-prob -1.08 0 1 randN 0)
    set buffer-weev-prob-wo (gen-weev-prob -1.08 0 1 randN 1)
    set core-weev-prob-bo buffer-weev-prob-bo
    set core-weev-prob-wo buffer-weev-prob-wo
  ]

  ;If "treat-diff", value of pWeevil varies with harvest treatment ('core') and species only
  if weevil-scenario = "treat-diff" [
    set buffer-weev-prob-bo (gen-weev-prob -1.08 0 0 0 0)
    set buffer-weev-prob-wo (gen-weev-prob -1.08 0 0 0 1)
    set core-weev-prob-bo (gen-weev-prob -1.08 1 0 0 0)
    set core-weev-prob-wo (gen-weev-prob -1.08 1 0 0 1)
  ]

  ;If "yearly-treat-diff", value of pWeevil varies with species, harvest treatment, and year
  if weevil-scenario = "yearly-treat-diff" [
    let randN random-normal 0 1.01
    set buffer-weev-prob-bo (gen-weev-prob -1.08 0 1 randN 0)
    set buffer-weev-prob-wo (gen-weev-prob -1.08 0 1 randN 1)
    set core-weev-prob-bo (gen-weev-prob -1.08 1 1 randN 0)
    set core-weev-prob-wo (gen-weev-prob -1.08 1 1 randN 1)
  ]

  ;Special case when testing for sensitivity of model to pWeevil; intercept is varied
  if weevil-scenario = "sensitivity" [
    set buffer-weev-prob-bo (gen-weev-prob (item 1 sens-params) 0 0 0 0)
    set buffer-weev-prob-wo (gen-weev-prob (item 1 sens-params) 0 0 0 1)
    set core-weev-prob-bo buffer-weev-prob-bo
    set core-weev-prob-wo buffer-weev-prob-wo
  ]

  ;Iterate throw mast-year index
  set mast-year-index (mast-year-index + 1)
  if mast-year-index = 9 [set mast-year-index 0]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Set effect of dispersal-scenario (dispersal, caching, and acorn survival parameters:
  ;pDispersal, pUndispEaten, pCache, dispDist, pDispEaten

  ;Parameter values are bundled together into a list

  ;If "custom", use values provided as input by user
  if dispersal-scenario = "custom" [
    set core-acorn-params-bo (list disperse-prob disperse-prob weibSc disperse-eaten-prob disperse-eaten-prob undisp-eaten-prob)
    set core-acorn-params-wo core-acorn-params-bo
    set buffer-acorn-params-bo core-acorn-params-bo
    set buffer-acorn-params-wo core-acorn-params-bo
  ]

  ;The following four scenarios make use of a series of regression models fit using HEE data
  ;The models can be found in files 'parameter_acorn_dispersal.R', 'parameters_acorn_fate.R', and 'model_acorn_dispersaldist.R' in S3 Regression Models
  ;Different probabilities are specificied for (1) different species and
  ;(2) whether or not the acorn fell in a "harvest treatment"/core area (i.e., shelterwood/midstory removal) or an unharvested/buffer area
  ;See SOEL manuscript, section "Harvest and Yearly-Varying Impacts on Seed Predation" for more details

  ;If "fixedaverage" use average value across all HEE data (basically, the transformed regression model intercepts)
  if dispersal-scenario = "fixedaverage" [
    ;Set parameters to values returned by gen-acorn-params
    ;gen-acorn-params uses the fitted regression models to return parameter values depending on the given scenario
    set buffer-acorn-params-bo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 0 0 0)
    set buffer-acorn-params-wo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 0 0 1)
    set core-acorn-params-bo buffer-acorn-params-bo
    set core-acorn-params-wo buffer-acorn-params-wo
  ]

  ;If 'yearly-diff', value of dispersal parameters vary with year (combines random year and mast availability effects) and species only
  if dispersal-scenario = "yearly-diff" [
    let randN random-normal 0 0.6322
    set buffer-acorn-params-bo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 1 randN 0)
    set buffer-acorn-params-wo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 1 randN 1)
    set core-acorn-params-bo buffer-acorn-params-bo
    set core-acorn-params-wo buffer-acorn-params-wo
  ]

  ;If "treat-diff", value of dispersal parameters varies with harvest treatment ('core') and species only
  if dispersal-scenario = "treat-diff" [
    set buffer-acorn-params-bo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 0 0 0)
    set buffer-acorn-params-wo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 0 0 1)
    set core-acorn-params-bo (gen-acorn-params 0.519 2.071 1.496 -0.179 1 0 0 0)
    set core-acorn-params-wo (gen-acorn-params 0.519 2.071 1.496 -0.179 1 0 0 1)
  ]

  ;If "yearly-treat-diff", value of dispersal parameters varies with species, harvest treatment, and year
  if dispersal-scenario = "yearly-treat-diff" [
    let randN random-normal 0 0.6322
    set buffer-acorn-params-bo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 1 randN 0)
    set buffer-acorn-params-wo (gen-acorn-params 0.519 2.071 1.496 -0.179 0 1 randN 1)
    set core-acorn-params-bo (gen-acorn-params 0.519 2.071 1.496 -0.179 1 1 randN 0)
    set core-acorn-params-wo (gen-acorn-params 0.519 2.071 1.496 -0.179 1 1 randN 1)
  ]

  ;Special case when testing for sensitivity of model to dispersal parameters; intercept is varied
  if dispersal-scenario = "sensitivity" [
    set buffer-acorn-params-bo (gen-acorn-params (item 2 sens-params) (item 3 sens-params) (item 5 sens-params) (item 6 sens-params) 0 0 0 0)
    set buffer-acorn-params-wo (gen-acorn-params (item 2 sens-params) (item 3 sens-params) (item 5 sens-params) (item 6 sens-params) 0 0 0 1)
    set core-acorn-params-bo buffer-acorn-params-bo
    set core-acorn-params-wo buffer-acorn-params-wo
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Set effect of browse-scenario, which controls browse probability on oak seedlings (parameter pBrowse)
  ;See file 'parameter_seedling_herbivory.R' in S3 Regression Models for more details

  ;If "custom", use value provided by user as input
  if browse-scenario = "custom" [ ]

  ;If "fixedaverage" use mean browse rate on seedlings from HEE data
  if browse-scenario = "fixedaverage" [set prob-browsed 0.1058]

  ;If "hee", calculate random year effect. Browse probability is calculated later on a per-seedling basis because height is a covariate
  if browse-scenario = "hee" [set br-year-ef random-normal 0 0.59]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Set effect of seedling-scenario, which controls oak seedling growth and survival (parameters meanGr and pSurv)

  ;The following two scenarios make use of a series of regression models fit using HEE data
  ;The models can be found in files 'parameter_seedling_survival.R' and 'parameter_seedling_growth.R' in S3 Regression Models
  ;See SOEL manuscript, section "Interactions of Drought and Harvest" for more details

  ;If "fixedaverage" use parameters from model fit with data from from both drought and non-drought years
  if seedling-scenario = "fixedaverage" [
    set surv-params (list -0.6 0.101 0.366 0.576) ; intercept species canopy age
    set growth-params (list 1.24 0.248 -0.94 -0.488 0.116 1.188) ; intercept seedSD browse canopy species obsSD
  ]

  ;If "randomdrought" there are two models, one fit with drought year data and one with non-drought year data
  ;Drought years are chosen randomly based on specified probability drought-prob and the appropriate models for growth and survival applied
  if seedling-scenario = "randomdrought" [
    ;drought conditions
    ifelse random-float 1 < drought-prob [
      set surv-params (list -0.531 0.197 0.528 0.44)
      set growth-params (list 0.911 0.157 -0.871 -0.238 0.085 1.016)]
    [;non-drought
      set surv-params (list 2.596 0 -0.733 0)
      set growth-params (list 2.091 0.199 -0.976 -1.733 0.468 1.468)]
  ]

  ;Special case when testing for sensitivity of model to dispersal parameters; intercept is varied
  if seedling-scenario = "sensitivity" [
    set growth-params (list (item 8 sens-params) 0.248 -0.94 -0.488 0.116 1.188)
    set surv-params (list (item 9 sens-params) 0.101 0.366 0.576)
  ]

end


;Function that generates weevil infestation proability parameter pWeevil, used in set-scenario above
;Inputs are intercept, two binary values indicating if the model inclues treatment effects and/or yearly effects,
;the value of the year effect, and the oak species
;See file ;parameter_weevil_infestation.R' in S3 Regression Models for more details
to-report gen-weev-prob [intercept treat yearly yearef spec]

  let weevmean intercept + (treat * -0.4438) + (yearly * (0.7324 * mast-mean-comb + yearef)) + (spec * -0.8249)
  let prob (1 + exp(-1 * weevmean)) ^ (-1)
  report prob

end


;Function that generates a set of acorn dispersal parameters, used in set-scenario above
;Inputs are removal model intercept, dispersal dist intercept, dispersed eaten intercept, undispersed eaten intercept,
;Two binary values indicating if the model includes treatment effects and/or yearly effects, the value of the year effect, and the oak species
;Parameter values are then generated from regression models fit from HEE data
;See files 'parameter_acorn_dispersal.R' and 'parameter_acorn_fate.R' in S3 Regression Models for more details on the models
to-report gen-acorn-params [remint distint deint ueint treat yearly yearef spec]

  let rem-mean remint + (treat * 0.129) + (yearly * yearef) + (spec * -0.162)
  let rem (1 + exp(-1 * rem-mean)) ^ (-1)
  let remw-mean rem-mean - 2.00
  let rem-weev (1 + exp(-1 * remw-mean)) ^ (-1)
  let dist exp(distint + (treat * -0.115) + (yearly * 0.114 * mast-mean-comb))
  let de-mean deint + (treat * 0.5817) + (yearly * 1.1414 * mast-mean-comb)
  let de (1 + exp(-1 * de-mean)) ^ (-1)
  let de-cached-mean de-mean - 5.4178
  let de-cached (1 + exp(-1 * de-cached-mean)) ^ (-1)
  let ue-mean ueint + (treat * 1.609) + (spec * 1.4714) + (yearly * 1.2051 * mast-mean-comb)
  let ue (1 + exp(-1 * ue-mean)) ^ (-1)

  ;Output parameter value list
  report (list rem rem-weev dist de de-cached ue) ;pRemoval, pRemoval given weeviled, weibSh, pDispEaten, pDispEatencache, pUndispEaten
  ;caching probability is calculated separately

end


;Function to tell a given mature oak tree how many acorns to produce in a time step
;See S1 Soel Description section 4.1 for more details
;Also see files 'parameter_acorn_production.R' and 'parameters_weevil_infestation.R' in S3 Regression Models
to produce-acorns

  ;Initialize acorns at 0 and grab black oak values for meanAcorn and pWeevil values
  let acorns-produced 0 let mast-mean mast-mean-bo let weevil-probability bo-weevil-prob

  ;If tree is a white oak, change to alternate set of parameters
  if species = "WO" [
    set mast-mean mast-mean-wo set weevil-probability wo-weevil-prob
    ]

  ;Using meanAcorn, calculate total acorns produced based on canopy area of tree
  set acorns-produced (pi * canopy-radius ^ 2 * random-exponential mast-mean)

  ;Keep a running count of total acorns produced for each species across all trees in the 'core' area
  if in-core = TRUE [
    set acorn-count (acorn-count + acorns-produced)
    ifelse species = "BO" [set bo-acorn-count (bo-acorn-count + acorns-produced)]
    [set wo-acorn-count (wo-acorn-count + acorns-produced)]
    ]

  ;If weevil-scenario is such that pWeevil is not a fixed, constant value, set weevil probability based on
  ;oak species and whether or not the tree is in a recently shelterwood-harvested area
  ;See SOEL manuscript, section "Harvest and Yearly-Varying Impacts on Seed Predation" for more details
  if weevil-scenario != "custom" and weevil-scenario != "sensitivity" [

   ifelse species = "BO" [set weevil-probability buffer-weev-prob-bo][set weevil-probability buffer-weev-prob-wo]
   if in-core = TRUE and harvest-type = "shelterwood" and ticks > burnin [
     ifelse species = "BO" [set weevil-probability core-weev-prob-bo][set weevil-probability core-weev-prob-wo]
     ]
  ]

  ;Set some temporary values for use later
  let tree-radius canopy-radius let temp species let coretemp in-core

  ;Create a number of new acorn agents equal to the number of acorns produced
  hatch-acorns acorns-produced [
    set species temp ;Match species to tree
    set in-core coretemp ;If tree is in core area, so is acorn
    set hidden? TRUE ;Hide in UI to make model run faster
    right random 360  ;;drop produced acorns under canopy randomly
    forward (random-float (tree-radius - 0.5)) + 0.5

    ;;Check to see if the acorn is weeviled based on pWeevil
    ;See S1 SOEL Description section 4.2 for more details
    ifelse random-float 1 < weevil-probability [set weevil TRUE] [set weevil FALSE]
  ]

end


;Function to control dispersal and caching of individual acorns
;See S1 SOEL Description section 4.2 for more details
;Also see files 'parameter_acorn_dispersal.R' and 'parameters_acorn_fate.R' in S3 Regression Models
to disperse-mast

  ;Set up parameter list depending on species
  ;List contains: pRemoval, pRemoval given weeviled, weibSh, pDispEaten, pDispEatencache, pUndispEaten
  ;Different lists depending on species and whether or not the acorn is in the harvested core area
  let core-acorn-params core-acorn-params-bo
  let buffer-acorn-params buffer-acorn-params-bo
  if species = "WO" [
    set core-acorn-params core-acorn-params-wo
    set buffer-acorn-params buffer-acorn-params-wo
  ]

  ;By default, acorns are in buffer/unharvested area
  let acorn-params buffer-acorn-params
  ;If they are in a recently harvested shelterwood/midstory removal area, switch parameter list
  if harvest-type = "shelterwood" and ticks > burnin and in-core = TRUE [set acorn-params core-acorn-params]

  ;Set dispersal probability pDispersal (i.e. removal probability) for a given acorn; depends on if it is weeviled
  let rem-prob 0
  ifelse weevil = FALSE [set rem-prob item 0 acorn-params][set rem-prob item 1 acorn-params]

  ;Check if acorn is dispersed using pDispersal
  ifelse random-float 1 < rem-prob [

    ;If acorn is dispersed, do the following:
    right random 360 ;Set azimuth in random direction
    set disp-dist random-weibull 1.400 (item 2 acorn-params)
    forward disp-dist ;Move on azimuth a specified distance (dispDist; random Weibull) based on HEE data

    ;Re-check to see if acorn is still in core area after being dispersed, and change parameter list if necessary
    check-in-core
    ifelse harvest-type = "shelterwood" and ticks > burnin and in-core = TRUE [set acorn-params core-acorn-params]
    [set acorn-params buffer-acorn-params]

    ;Caching probability pCache is calculated using a regression model based on HEE data
    ;See file 'parameters_acorn_fate.R' in S3 Regression Models for more details
    ;Depends on dispersal-scenario like other acorn fate parameters, and also on dispersal distance
    let pcache 0

    if dispersal-scenario = "yearly-diff" or dispersal-scenario = "yearly-treat-diff" [
      let cache-mean -2.4986 + 0.08858 * disp-dist + random-normal 0 1.663
      set pcache (1 + exp(-1 * cache-mean)) ^ (-1)]

    if dispersal-scenario = "fixedaverage" or dispersal-scenario = "treat-diff"[
      let cache-mean -2.4986 + 0.08858 * disp-dist
      set pcache (1 + exp(-1 * cache-mean)) ^ (-1)]

    if dispersal-scenario = "sensitivity" [
      let cache-mean (item 4 sens-params) + 0.08858 * disp-dist
      set pcache (1 + exp(-1 * cache-mean)) ^ (-1)]

    if dispersal-scenario = "custom" [set pcache cache-prob]

    ;Check if acorn is cached and if it survives (i.e., if it is not eaten) using parameter pDispEaten
    ifelse random-float 1 < pcache [
      set cached TRUE
      if random-float 1 < (item 4 acorn-params) [die]
    ] [
      set cached FALSE
      if random-float 1 < (item 3 acorn-params) [die]
      ]
  ]

  ;If acorn is not dispersed: check if it survives (i.e., is not eaten) using parameter pUndispEaten
  [if random-float 1 < (item 5 acorn-params) [die]]

end


;Function to determine if acorns surviving the dispersal process germinate into seedlings
;See S1 SOEL Description section 4.2 for more details
to germinate-mast

  let temp species

  ;Germination probability pGerm depends on caching and weevil status
  ifelse cached = TRUE [
    ;Based on Haas and Heske 2005 (0.77 for buried)
    set germ germ-prob]
  ;;Based on:
  ;;Lombardo & McCarthy 2009 (0.26 germ for weeviled)
  ;;and Haas and Heske 2005 (0.09 for surfaced)
   [ifelse weevil = TRUE [set germ 0.09 * 0.26]
    [set germ 0.09]]

  ;Check if acorn germinates based on pGerm
  if random-float 1 < germ [
    ;If it does, generate a new seedling
    hatch-oaks 1 [
      set species temp ;species matches species of acorn
      set age 0 ;Initialize new seedling at age 0
      set size 1
      set seedling TRUE
      set hidden? TRUE ;Hide from UI
      set height 0.092 ;random initial height based on HEE seedling data
      set indeffect (random-normal 0 item 1 growth-params) ;Set inherent growth potential (i.e., individual random effect value on growth for use later)
    ]]

    die ;If acorn does not germinate, it dies and is removed from simulation (no seed bank)

end


;Function to generate height growth of a given oak seedling in a time step
;See S1 SOEL Description section 4.3 for more details
to grow-seedling

  ;Set available light
  set light (1 - [shade] of patch-here)

  ;Set oak species effect
  let sp-ef 0
  if species = "WO" [set sp-ef 1]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Calculate probability of being browsed, pBrowse
  let pbrowse 0

  if browse-scenario = "custom" or browse-scenario = "fixedaverage"[set pbrowse prob-browsed]

  ;Scenario in which browse probability varies with seedling height and year
  ;See file 'parameter_seedling_herbivory.R' in S3 Regression Models for more details
  if browse-scenario = "hee"[
    let br-mean -4.521 + 0.158 * (height * 100) - 0.00148 * (height * 100 * height * 100) + 0.372 * sp-ef + br-year-ef
    set pbrowse (1 + exp(-1 * br-mean)) ^ (-1)
  ]

  ;Special case when testing for sensitivity of model to dispersal parameters; intercept is varied
  if browse-scenario = "sensitivity"[
    let br-mean (item 7 sens-params) + 0.158 * (height * 100) - 0.00148 * (height * 100 * height * 100) + 0.372 * sp-ef
    set pbrowse (1 + exp(-1 * br-mean)) ^ (-1)
  ]

  ;Check if seedling is browsed
  ifelse random-float 1 < pbrowse [set browsed 1][set browsed 0]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Calculate seedling growth meanGr dependent on herbivory
  ;See file 'parameter_seedling_growth.R' in S3 Regression Models for more details
  ;Calculate mean growth meanGr (on neglog scale)
  let growthpred ((item 0 growth-params) + indeffect
  + (item 2 growth-params) * browsed + (item 3 growth-params) * (1 - light)
  + (item 4 growth-params) * sp-ef + (random-normal 0 item 5 growth-params))

  set actual-growth (inv-neglog growthpred) / 100 ;Convert growth back to normal scale with inv-neglog function

  if actual-growth < 0 [set actual-growth 0] ;Minimum growth is 0

  set height (height + actual-growth) ;Add new growth to old height to get new height

  ;If seedling now exceeds height of 1.37 m, it becomes a sapling and grows according to contextual forest submodel
  if height > 1.37 [
    set dbh max list 0 ((-1.88 + 0.01372 * (height * 100)) / 100) ;Set new dbh based on height. When seedling reaches dbh 0.01, height = 2.039
    init-params ;Initialize sapling variables
  ]

end


;Function to determine if a seedling survives a given time step
;See S1 SOEL Description section 4.3 for more details
;Also see file 'parameter_seedling_survival.R' in S3 Regression Models
to check-seedling-survival ;Based on HEE data

  if age = 0 [if random-float 1 > 0.567 [die]] ;Initial seedling establishment probability based on HEE data

  ;Set up species effect
  let sp-ef 0
  if species = "WO" [set sp-ef 1]

  ;Calculate survival probability pSurv
  let survpred ((item 0 surv-params) + (item 1 surv-params) * sp-ef + (item 2 surv-params)
    * (1 - light) + (item 3 surv-params) * (min list 5 age))
  let psurv (1 + exp(-1 * survpred)) ^ (-1)

  ;Check if seedling survives time step
  if random-float 1 > psurv [die]

  ;If it does increase its age by 1 year
  set age age + 1

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                Light Calculation Procedures                     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function that calculates light available to a given tree as well as density of nearby stems
;See S1 SOEL Description sections 5.1-5.2
to calc-light

 ;First ask patches to reset base shade, stem density, and color to default
 ask patches [set pcolor brown set stem-dens 0 set shade 0]

 ;Iterate through all saplings and mature trees in order of descending height (i.e., tallest tree first)
 foreach sort-by [[height] of ?1 > [height] of ?2] turtles with [seedling = FALSE] [
    ask ? [

      let start-ycor ycor ;Save initial y-coordinate of tree
      let temp canopy-density ;Save canopy density of tree (depends on k, species, and dbh)
      ;Calculate light available to tree as 1 - [mean(shade) of patches within the tree's canopy]
      ;Tree is shaded only by trees that are taller than itself
      set light (1 - mean [shade] of patches in-radius max list 3 canopy-radius)

      ;Density parameter is number of nearby taller trees; will penalize growth to control overshot in basal area
      set density [stem-dens] of patch-here
      ask patches in-radius density-dep [set stem-dens stem-dens + 1] ;Add this tree to current density estimate to effect shorter nearby trees
      ;For choice of radius value, see S1 SOEL Description Figure 4

      ;Draw canopy of this tree and add its shade to the patches it effects
      ;Shape of shade projection is a series of 3 concentric shapes (one circle and 2 ellipses) - see S1 SOEL Description Figure 3
      ;This is based on ShadeMotion simulation for 39 degrees latitude

      ;If tree has a very small canopy radius, only shades innermost circle centered on tree
      ifelse canopy-radius < 2 [
        ;Calculate overlapping shade instead of summing up leaf-weight; mathematically equivalent and fewer calculations to make.
        ask patches in-radius canopy-radius [set shade (shade + (1 - shade) * temp) ]]

      [
      ;If tree has larger canopy radius, tree shades over all 3 shapes

      ;Shade inner circle
      set ycor (ycor + canopy-radius / 2) ;shift center of shaded shape to the north slightly
      ask patches in-radius canopy-radius [
        set in-layer1 TRUE
        set shade (shade + (1 - shade) * temp)]

      ;Shade first ellipse (minus the initial canopy)
      set ycor (ycor + canopy-radius / 2.5) ;shift center of shape north again
      let test1 in-ellipse (canopy-radius * 2) (canopy-radius * 1.1) 1
      ask test1 [
        set in-layer2 TRUE
        set shade (shade + (1 - shade) * temp * 0.5)] ;shade is only 50% as strong in this ellipse

      ;Shade second ellipse (minus the first ellipse)
      set ycor (ycor + canopy-radius / 4.5) ;Shift north again
      let test2 in-ellipse (canopy-radius * 3) (canopy-radius * 1.5) 2
      ask test2 [
        set shade (shade + (1 - shade) * temp * 0.25)] ;shade is only 25% as strong in this ellipse
      ;reset layering
      let test3 in-ellipse (canopy-radius * 3) (canopy-radius * 1.5) 3
      ask test3 [set in-layer1 FALSE set in-layer2 FALSE]
      ]

      ;move tree back to starting location
      set ycor start-ycor

 ]]
end

;Function equivalent to in-radius that selects patches in an ellipse shape
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


;Helper functions for in-ellipse
to-report xdistance [other-patch]
  let xdiff abs (pxcor - [pxcor] of other-patch)
  report min (list xdiff (world-width - xdiff))
end

to-report ydistance [other-patch]
  let ydiff abs (pycor - [pycor] of other-patch)
  report min (list ydiff (world-height - ydiff))
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;                       Utility Procedures                        ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Function to generate a random Weibull value based on provided parameters
to-report random-weibull [Wshape Wscale]
  let R random-float 1
  report Wscale * (- ln(R)) ^ (1 / Wshape)
end

;Function to check if agent is in core or buffer area
to check-in-core
  ifelse xcor < xcutoff and xcor > (-1 * xcutoff) and ycor < ycutoff and ycor > (-1 * ycutoff)[set in-core TRUE][set in-core FALSE]
end

;Function to back-convert neglog transformed variables to original scale
to-report inv-neglog [x] ;neg-log transformation function
  ifelse x <= 1 [report (1 - exp(-1 * x))][report (exp(x) - 1)]
end

;Function to calculate site quality for a given species based on environmental characteristics
to calc-site-quality

  ;Skip calculations if site quality is specified manually by user
  ifelse manual-site-qual = TRUE [

    set sitequal-woak sqwoak
    set sitequal-boak sqboak
    set sitequal-maple sqmap
    set sitequal-poplar sqpop

  ][

  ;For each species, calculate values for individual response functions and then multiply together
  ;Parameters based bon Botkin 1993 and Holm 2012 with some guesses

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
  ;Simulate overstory of new forest stand
  ;Create adult trees
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

  ;Create saplings
  create-oaks dens-adjust * n-sap-oak [
    set dbh max list 0.015 ((random-normal 14.9 5) / 100)
    ifelse random-float 1 > 0.5 [set species "WO"][set species "BO"]
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

;Function that initializes newly created trees (sapling and mature) with various required parameter values
to init-params

  ifelse dbh < 0.1 [set shape "square"] [set size 2 set shape "tree"] ;Set appropriate icon shape
  set hidden? FALSE ;Saplings and mature trees are visible in the UI
  set min-increment 0.01 ;Minimum growth increment to not get extra mortality penalty
  set actual-growth 0.02 ;Initialize actual growth to be something above the minimum
  set growth-mortality 0.369 ;Extra mortality; see S1 SOEL Description equation 9
  set age 1 ;Start at age 1
  set seedling FALSE ;Not a seedling

  ;Species-specific JABOWA required variables related to growth and survival

  ;Oaks
  if breed = oaks [
    ifelse species = "WO" [ ;White oak based on Botkin 1993 and Holm 2012
      set color white
      set Dmax 100 set Hmax 3800 ;based on HEE data
      set Amax 400 set intrinsic-mortality 4.0 / Amax ;Based on 2% reaching max age; common to all species
      set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
      set C 1.75
      set G 104
      set light-tol "intermediate"
      set acorn-mean random-exponential 10 ;mean oak production / m2 based on HEE histogram

    ][set color black ;Black oak based on Botkin 1993 and Holm 2012
      set Dmax 100 set Hmax 3800 ;based on HEE data
      set Amax 300 set intrinsic-mortality 4.0 / Amax ;Based on 2% reaching max age; common to all species
      set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
      set C 1.75
      set G 122
      set light-tol "intermediate"
      set acorn-mean random-exponential 10 ;mean oak production / m2 based on HEE histogram
    ]]

  ;Sugar Maple
  if breed = maples [ ;Sugar maple based bon Botkin 1993 and Holm 2012
    set color sky
    set Dmax 100 set Hmax 3350 ;set Dmax 170
    set Amax 400 set intrinsic-mortality 4.0 / Amax
    set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
    set C 1.57
    set G 118.7
    set light-tol "high"
  ]

  ;Tulip Poplar
  if breed = poplars [ ;Tulip poplar based on Holm 2012 with some guesses
    set color red
    set Dmax 100 set Hmax 4000
    set Amax 300 set intrinsic-mortality 4.0 / Amax
    set b2 2 * (Hmax - 137) / (Dmax) set b3 (Hmax - 137) / (Dmax ^ 2)
    set C 1.75 ;assumed to be similar to oak
    set G 140
    set light-tol "low"
  ]

  ;Use allometric equations to generate other characteristics from dbh
  allometerize dbh

end


;Catch-all function that calculates a variety of global simulation metrics at the end of each time step
to calc-global-vars

  let adjust (x-core * y-core) / 10000 ;Adjustment factor to convert other variables to per ha scale depending on size of simulation

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
  set new-bo-seedlings (count turtles with [seedling = TRUE and species = "BO" and age = 1 and in-core = TRUE]) / adjust
  set new-wo-seedlings (count turtles with [seedling = TRUE and species = "WO" and age = 1 and in-core = TRUE]) / adjust
  set seedlings-class1 (count turtles with [seedling = TRUE and height <= 0.3 and in-core = TRUE]) / adjust
  set seedlings-class2 (count turtles with [seedling = TRUE and height > 0.3 and height <= 0.6 and in-core = TRUE]) / adjust
  set seedlings-class3 (count turtles with [seedling = TRUE and height > 0.6 and height <= 1.4 and in-core = TRUE]) / adjust
  set seedlings-class123 (count turtles with [seedling = TRUE and height <= 1.4 and in-core = TRUE]) / adjust
  set seedbo-class123 (count turtles with [seedling = TRUE and species = "BO" and height <= 1.4 and in-core = TRUE]) / adjust
  set seedwo-class123 (count turtles with [seedling = TRUE and species = "WO" and height <= 1.4 and in-core = TRUE]) / adjust
  set seedlings-class4 (count oaks with [height > 1.4 and dbh < 0.015 and in-core = TRUE]) / adjust
  set seedbo-class4 (count oaks with [height > 1.4 and species = "BO" and dbh < 0.015 and in-core = TRUE]) / adjust
  set seedwo-class4 (count oaks with [height > 1.4 and species = "WO" and dbh < 0.015 and in-core = TRUE]) / adjust
  set total-acorns round (acorn-count / adjust)
  set total-bo-acorns round (bo-acorn-count / adjust)
  set total-wo-acorns round (wo-acorn-count / adjust)
  ifelse count oaks with [dbh > 0.2 and in-core = TRUE] > 0 [set acorns-pertree (total-acorns / count oaks with [dbh > 0.2 and in-core = TRUE])]
  [set acorns-pertree 0]
  ifelse total-acorns > 0 [set pct-germ (new-seedlings / total-acorns)][set pct-germ 0]
  ifelse total-bo-acorns > 0 [set pct-bo-germ (new-bo-seedlings / total-bo-acorns)][set pct-bo-germ 0]
  ifelse total-wo-acorns > 0 [set pct-wo-germ (new-wo-seedlings / total-wo-acorns)][set pct-wo-germ 0]
  set regen-dens (count oaks with [height >= 1.4 and dbh < 0.015 and in-core = TRUE]) / adjust
  set regen-stump-dens (count oaks with [sprout? = TRUE and height >= 1.4 and dbh < 0.015 and in-core = TRUE]) / adjust

end


;Function to color patches in the UI based on the amount of shade they have
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

;Function to harvest the simulated forest as required
;See S1 SOEL Description section 5.5
to conduct-harvest

  ;If there is no harvest chosen by the user, end the function
  if harvest-type = "none" [stop]

  ;If there is a harvest but it's not the correct year to harvest, end the function
  if (ticks + 1) != harvest-year [stop]

  ;Now move forward assuming it is a harvest year

  ;Calculate adjustment factor to convert variables to per ha scale later
  let adjust (x-core * y-core) / 10000

  ;If harvest is "clearcut", tell all trees in the core area larger than 1 cm dbh to create a sprout or die
  if harvest-type = "clearcut" [
    ask turtles with [dbh > 0.01 and in-core = TRUE] [create-sprout]
  ]

  ;If harvest is "singletree", harvest every 20 years throughout the core area down to a min basal area of 25 m2/ha
  if harvest-type = "singletree" [

    set basal-area (sum [ba] of turtles with [dbh >= 0.01 and in-core = TRUE]) / adjust ;Calculate current basal area
    set harvest-year harvest-year + 20 ;Set up next harvest time in 20 years

    ;Iterate, harvesting one large (>10 cm dbh) tree in the core at a time, until minimum limit is reached
    ;Possible to change the minimum dbh size harvested below
    if basal-area >= 25 [
      loop [
        let potential one-of turtles with [dbh >= 0.10 and in-core = TRUE]
        ask potential [create-sprout] ;Ask tree to die
        set basal-area (sum [ba] of turtles with [dbh >= 0.01 and in-core = TRUE]) / adjust ;Calculate new basal area with tree removed
        if basal-area <= 25 [stop] ;Stop when minimum is reached
        ]
    ]
  ]

  ;If harvest is "shelterwood" conduct a 3-phase shelterwood harvest
  if harvest-type = "shelterwood" [

    ;If in first phase
    ifelse shelter-phase = 1 [

      ask turtles with [breed != oaks and dbh <= 0.254 and in-core = TRUE] [die] ;Harvest midstory non-oaks and kill with herbicide
      set shelter-phase 2 ;Set phase to 2
      set harvest-year (harvest-year + 7) ;Set year when phase 2 will occur

    ]
    [
      ;If in 2nd phase
      ifelse shelter-phase = 2 [

        ;Inddividually remove overstory trees down to minimum basal area of 16.1
        ;Generally, only large oaks should be left
        if basal-area >= 16.1 [
          loop [
            let potential min-one-of turtles with [age > 20 and in-core = TRUE] [light]
            ask potential [ifelse breed = oaks [create-sprout][die]] ;Only harvested oaks allowed to sprout
            set basal-area (sum [ba] of turtles with [dbh >= 0.01 and in-core = TRUE]) / adjust
            if basal-area <= 16.1 [stop]
          ]
        ]
        set shelter-phase 3 ;Set phase to 3
        set harvest-year (harvest-year + 8) ;Set year when phase 3 will occur
      ]
      [
        ;In final phase, remove all remaining overstory trees
        ask turtles with [age > 20 and in-core = TRUE] [create-sprout]
        set shelter-phase 1 ;Set phase back to 1 in case harvest needs to happen again
      ]

  ]]

end
@#$#@#$#@
GRAPHICS-WINDOW
167
12
882
748
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
23
41
86
74
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
93
41
156
74
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
467
909
642
942
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
468
947
640
980
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
468
987
640
1020
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
14
573
82
618
BA (m2/ha)
basal-area
2
1
11

SLIDER
794
811
910
844
bo-weevil-prob
bo-weevil-prob
0
1
0.625
0.01
1
NIL
HORIZONTAL

SLIDER
795
925
907
958
germ-prob
germ-prob
0
1
0.79
0.01
1
NIL
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
14
622
81
667
qDBH (cm)
qdbh
2
1
11

MONITOR
89
573
157
618
BA (ft2/ac)
basal-area-ft
2
1
11

MONITOR
88
623
153
668
qDBH (in)
qdbh-in
2
1
11

MONITOR
14
672
78
717
Trees/Ha
dens
2
1
11

MONITOR
88
673
151
718
Trees/Ac
dens-ac
2
1
11

PLOT
894
640
1054
760
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
1056
640
1234
760
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
469
1025
641
1058
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
222
1015
299
1060
NIL
sitequal-woak
2
1
11

MONITOR
382
1014
459
1059
NIL
sitequal-maple
2
1
11

MONITOR
300
1014
380
1059
NIL
sitequal-poplar
2
1
11

TEXTBOX
229
878
379
896
Site Conditions
14
0.0
1

TEXTBOX
60
199
210
217
Scenarios
14
0.0
1

TEXTBOX
295
71
445
89
Output
14
0.0
1

TEXTBOX
35
84
185
102
Harvest Controls
14
0.0
1

CHOOSER
17
108
155
153
harvest-type
harvest-type
"none" "clearcut" "shelterwood" "singletree"
1

TEXTBOX
54
16
204
34
Start Model
14
0.0
1

SLIDER
22
773
194
806
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
23
811
195
844
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
49
752
199
770
Tuning Parameters
14
0.0
1

SWITCH
226
903
373
936
manual-site-qual
manual-site-qual
0
1
-1000

SLIDER
225
942
317
975
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
323
941
415
974
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
225
977
317
1010
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
322
977
414
1010
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
194
63
366
96
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
193
102
365
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
193
140
365
173
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
20
158
155
191
burnin
burnin
0
100
5
1
1
years
HORIZONTAL

CHOOSER
946
788
1084
833
seedlings
seedlings
"none" "hee"
1

CHOOSER
20
220
158
265
mast-scenario
mast-scenario
"random" "hee" "fixedaverage" "fixedgood" "fixedbad" "priorgood" "priorbad" "custom" "sensitivity" "priordifference"
2

SWITCH
796
1000
904
1033
sprouting
sprouting
0
1
-1000

SLIDER
672
847
786
880
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
672
886
788
919
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
795
888
908
921
cache-prob
cache-prob
0
1
0.286
0.001
1
NIL
HORIZONTAL

SLIDER
672
923
786
956
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
673
1000
788
1033
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
19
368
158
413
seedling-scenario
seedling-scenario
"custom" "fixedaverage" "randomdrought" "sensitivity"
1

SLIDER
34
471
142
504
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
19
416
158
461
browse-scenario
browse-scenario
"custom" "fixedaverage" "hee" "sensitivity"
1

SLIDER
673
811
791
844
wo-weevil-prob
wo-weevil-prob
0
1
0.3679
0.01
1
NIL
HORIZONTAL

CHOOSER
19
269
158
314
weevil-scenario
weevil-scenario
"custom" "fixedaverage" "yearly-diff" "treat-diff" "yearly-treat-diff" "sensitivity"
1

CHOOSER
19
319
158
364
dispersal-scenario
dispersal-scenario
"custom" "fixedaverage" "yearly-diff" "treat-diff" "yearly-treat-diff" "sensitivity"
1

SLIDER
673
776
765
809
mast-val
mast-val
0
50
11
1
1
NIL
HORIZONTAL

SLIDER
794
848
886
881
weibSc
weibSc
4
12
5.89
0.01
1
NIL
HORIZONTAL

TEXTBOX
676
755
826
773
Custom Oak Params
14
0.0
1

PLOT
903
265
1208
385
New Seedlings
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"All" 1.0 0 -2674135 true "" "plot new-seedlings"
"BO" 1.0 0 -16777216 true "" "plot new-bo-seedlings"
"WO" 1.0 0 -7500403 true "" "plot new-wo-seedlings"

PLOT
905
512
1211
632
Sapling Density
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"All" 1.0 0 -2674135 true "" "plot seedlings-class4"
"BO" 1.0 0 -16777216 true "" "plot seedbo-class4"
"WO" 1.0 0 -7500403 true "" "plot seedwo-class4"

PLOT
905
388
1210
508
Seedling Density
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"All" 1.0 0 -2674135 true "" "plot seedlings-class123"
"BO" 1.0 0 -16777216 true "" "plot seedbo-class123"
"WO" 1.0 0 -7500403 true "" "plot seedwo-class123"

PLOT
903
138
1207
258
Percent Germination
NIL
NIL
0.0
10.0
0.0
0.03
true
true
"" ""
PENS
"All" 1.0 0 -2674135 true "" "plot pct-germ"
"BO" 1.0 0 -16777216 true "" "plot pct-bo-germ"
"WO" 1.0 0 -7500403 true "" "plot pct-wo-germ"

SLIDER
672
960
786
993
mean-growth
mean-growth
0
100
2.45
0.05
1
cm
HORIZONTAL

SLIDER
794
960
906
993
mean-survival
mean-survival
0
1
0.35
0.01
1
NIL
HORIZONTAL

PLOT
904
10
1206
130
Acorns Produced
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"All" 1.0 0 -2674135 true "" "plot total-acorns"
"BO" 1.0 0 -16777216 true "" "plot total-bo-acorns"
"WO" 1.0 0 -7500403 true "" "plot total-wo-acorns"

@#$#@#$#@
## WHAT IS IT?

SOEL (Simulating Oak Early Life history) is an individual-based model of early oak life history, modeling processes from acorn production through seedling growth and survival wihle tracking individual acorns, seedlings, and trees. These processes occur within a surrounding forest simulated using a forest gap model adapted from the classic JABOWA framework. The model allows us to manipulate early oak life history parameters (e.g., rates of weevil infestation in acorns or herbivory on seedlings) and determine the implications for metrics of oak regeneration (e.g. seedling density). Parameter estimates for the early life history portion of the model are based on data for black and white oak (Quercus velutina, Q. alba) collected at the Hardwood Ecosystem Experiment (HEE) in southern Indiana, USA. The simulated forest is composed not only of oak, but also a generic shade-tolerant competitor ("maple", blue) and shade-intolerant competitor ("tulip poplar", red). For a much more detailed description of the model, see supporting information S1 SOEL Description.

## HOW TO USE IT

  **1. Setup the Initial Forest**
First, define the dimensions of your simulated forest (in meters) using the "x-core" and "y-core" sliders. The forest is a torus (that is, the edges wrap around to the opposite side); thus, to minimize edge effects, define a buffer width around the forest area using the buffer slider. The  buffer area will not be simulated but not included in output metric calculations. Second, the "Initial Forest" controls determine the composition of the forest at the beginning of the simulation. The default is based on collected Hardwood Ecosystem Experiment data, but custom values for the density of mature and sapling trees can be specified. Finally, you can define the site quality for the species in the simulation by changing variables under "Site Conditions" like available nitrogen, distance to the water table, etc. (identical to options in JABOWA). If you wish, you can set custom values for the two "Tuning Variables": the light extinction coefficient (k; higher values = less light under tree canopies) and the density-dependent mortality coefficient (higher values = greater density-dependent mortality of saplings and mature trees).

  **2. Setup the Harvest Regime**
First, define a "burnin" period for the simulated forest to stabilize; the first harvest will occur immeadiatly after this period is over. Next, define a harvest type: the options are "clearcut" (all trees > 1 cm dbh removed in one harvest), shelterwood (a 3-stage shelterwood harvest over ~20 years) or single-tree selection (large trees removed down to 25 m2/ha basal area every 20 years). Harvests are based on the HEE harvests (see Kalb and Mycroft [2013]).

  **3. Customize the Oak Regeneration Process**
On the left-hand side, there are several "Scenarios" that can be set for different parts of the oak lifecycle. The simplest way to customize the oak regeneration process is to set all scenarios to "custom" and then manually set values for the key oak life history variables under the heading "Custom Oak Params". Several other scenarios are also available, which correspond to the experiments described in the associated paper on SOEL. For example, parameters can be fixed to the average values observed at HEE ("fixedaverage") or vary with harvest ("treat-diff"), year ("yearly-diff"), or both ("treat-yearly-diff") based on HEE data (see S3 Regression Models). You can completely turn off early oak life history processes by setting "seedings" to "none". If you do this, oak saplings will be generated the same way tulip poplar and maple saplings are instead.

  **4. Start the Simulation**
Click the "Setup" button to apply the settings chosen above and populate the initial forest. Then, click "Go" to begin the simulation. Clicking "Go" again will pause. Each model "tick" represents one year of time. Brown represents completely unshaded ground, while progressively darker shades of green correspond to greater canopy cover from surrounding trees. Individual mature trees are shown with "tree" icons with the color corresponding to species (white and black are white and black oak, respectively; red is tulip poplar, blue is maple). Smaller squares represent saplings and smaller trees.

  **5. Monitor Output**
Various numeric and graphical monitors are provided to show key information about the state of the simulation over time, including forest structural characteristics like basal area, the number of acorns produced (and % that germinated), and the density of oak seedlings.

  **6. Run from R**
When running many replicate scenarios and/or simulations, the regular NetLogo interface is inefficient. the BehaviorSpace built-in tool can be helpful for this, but still isn't ideal, especially when you are outputting large amounts of data that will be analyzed elsewhere. Instead, we provide an R script ("sim_function.R") which uses package RNetLogo to setup and run simulations in parallel, and format the output. Simulation variables can be changed with arguments to the function in R.
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
NetLogo 5.3.1
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
  <experiment name="test1" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="40"/>
    <metric>basal-area</metric>
    <enumeratedValueSet variable="y-core">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wo-weevil-prob">
      <value value="0.2357"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="HEE-mean">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed-scenario">
      <value value="&quot;randomdrought&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-scenario">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="burnin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wilt">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disperse-eaten-prob">
      <value value="0.705"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="undisp-eaten-prob">
      <value value="0.538"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="available-N">
      <value value="325"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-type">
      <value value="&quot;shelterwood&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-maple">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bo-weevil-prob">
      <value value="0.3491"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disperse-prob">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqmap">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weevil-scenario">
      <value value="&quot;fixedaverage&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seedlings">
      <value value="&quot;hee&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqboak">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-maple">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="light-extinct">
      <value value="2.5E-4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density-dep">
      <value value="3.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="manual-site-qual">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqwoak">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cache-prob">
      <value value="0.288"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disperse-dist">
      <value value="5.185"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="x-core">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prob-browsed">
      <value value="0.1058"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-oak">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="germ-prob">
      <value value="0.77"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seedling-growth">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sprouting">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mast-scenario">
      <value value="&quot;hee&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-distrib">
      <value value="&quot;exponential&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sapling-poplar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sqpop">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DegDays">
      <value value="4840"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="browse-scenario">
      <value value="&quot;fixedaverage&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="drought-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wt-dist">
      <value value="9.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="buffer">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mature-poplar">
      <value value="0"/>
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
