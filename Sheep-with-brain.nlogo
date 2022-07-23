extensions [ ls rnd table profiler ]


globals [
  grass

  sheep-efficiency
  wolf-efficiency
  sheep-escape-efficiency

  sheep-eaten
  grass-eaten

  sheep-born
  wolves-born

  patches-with-sheep

  wolf-intake
  wolf-intake-attempts
  sheep-intake
  sheep-intake-attempts

  avg-grass
  avg-patches-with-sheep

  smoothed-values


  inputs
  input-descriptions
  outputs
  brain-pool
  layers

  inspectable-brain

  sheep-towards-grass
  sheep-towards-sheep
  sheep-towards-wolves
  wolves-towards-grass
  wolves-towards-sheep
  wolves-towards-wolves

  null-sheep-towards-grass
  null-sheep-towards-sheep
  null-sheep-towards-wolves
  null-wolves-towards-grass
  null-wolves-towards-sheep
  null-wolves-towards-wolves

  drag-target
]

;; Sheep and wolves are both breeds of turtle.
breed [sheep a-sheep]  ;; sheep is its own plural, so we use "a-sheep" as the singular.
breed [wolves wolf]
turtles-own [
  energy
  brain
  null-brain
]
patches-own [countdown]

to setup
  clear-all
  set brain-pool ls:models

  set smoothed-values table:make

  set sheep-intake []
  set wolf-intake []
  set wolf-intake-attempts []
  set sheep-intake-attempts []

;  set inputs (list
;    [ -> binary any? (in-vision-at patches (- fov / 3)) with [ pcolor = green ] ]
;    [ -> binary any? (in-vision-at patches 0) with [ pcolor = green ] ]
;    [ -> binary any? (in-vision-at patches (fov / 3)) with [ pcolor = green ] ]
;    [ -> binary any? other (in-vision-at sheep (- fov / 3)) ]
;    [ -> binary any? other (in-vision-at sheep 0) ]
  ;    [ -> binary any? other (in-vision-at sheep (fov / 3)) ]
  ;    [ -> binary any? other (in-vision-at wolves (- fov / 3)) ]
  ;    [ -> binary any? other (in-vision-at wolves 0) ]
  ;    [ -> binary any? other (in-vision-at wolves (fov / 3)) ]
  ;  )
  let viz-seg-size vision / granularity
  set inputs (reduce sentence
    (map [ min-dist ->
      reduce sentence map [ angle ->
        (list
          [ -> binary any? (in-vision-at patches angle min-dist (min-dist + viz-seg-size)) with [ pcolor = green ] ]
          [ -> binary any? other (in-vision-at sheep angle min-dist (min-dist + viz-seg-size)) ]
          [ -> binary any? other (in-vision-at wolves angle min-dist (min-dist + viz-seg-size)) ]
        )
      ] (range (fov / -2 + 15) (fov / 2 - 15 + 1) 30)
    ] (range 0 vision viz-seg-size))
  )
  set inputs lput [ -> 1 ] inputs ;; add bias

  set input-descriptions lput "bias" (reduce sentence
    (map [ min-dist ->
      reduce sentence map [ angle ->
        (list
          (word "g " (precision (min-dist + viz-seg-size) 1) " " angle)
          (word "s " (precision (min-dist + viz-seg-size) 1) " " angle)
          (word "w " (precision (min-dist + viz-seg-size) 1) " " angle)
        )
      ] (range (fov / -2 + 15) (fov / 2 - 15 + 1) 30)
    ] (range 0 vision viz-seg-size))
  )

  set outputs (list
    [-> lt 30]
    [->]
    [-> rt 30]
  )

  set layers (sentence (length inputs) hidden-nodes (length outputs))

  ask patches [
    set pcolor brown
    set countdown random grass-regrowth-time ;; initialize grass grow clocks randomly for brown patches
  ]
  ask n-of (initial-grass-density * count patches) patches [
    set pcolor green
    set countdown grass-regrowth-time
  ]
  set-default-shape sheep "sheep"
  create-sheep initial-number-sheep  ;; create the sheep, then initialize their variables
  [
    set color white
    set size 1.5  ;; easier to see
    set label-color blue - 2
    let b sheep-threshold / 2
    set energy b + random b
    setxy random-xcor random-ycor
  ]
  set-default-shape wolves "wolf"
  create-wolves initial-number-wolves  ;; create the wolves, then initialize their variables
  [
    set color black
    set size 2  ;; easier to see
    let b wolf-threshold / 2
    set energy b + random b
    setxy random-xcor random-ycor
  ]

  ask turtles [
    setup-brain
    add-stats
  ]

  set grass count patches with [pcolor = green]

  set drag-target nobody

  reset-ticks
end

;to test-vision
;  ask one-of sheep [
;

to sample-effs
  setup
  let n 10000
  let weff []
  let seff []
  let wpop []
  let spop []
  let gpop []

  let seff-escape []

  while [ ticks < n and any? turtles ] [
    set spop lput count sheep spop
    set wpop lput count wolves wpop
    set gpop lput grass gpop
    go
    set weff lput wolf-efficiency weff
    set seff lput sheep-efficiency seff
    set seff-escape lput sheep-escape-efficiency seff-escape
  ]
  let w-tick-mean mean weff
  let s-tick-mean mean seff
  let w-indiv-mean (sum (map * weff wpop)) / (sum wpop)
  let s-indiv-mean (sum (map * seff spop)) / (sum spop)

  let s-esc-tick-mean mean seff-escape
  let s-esc-indiv-mean safe-div (sum (map * seff-escape spop)) (sum spop)

  print (word "Sheep:   tick-mean=" (precision s-tick-mean 2) " indiv-mean=" (precision s-indiv-mean 2) " pop-std=" (precision standard-deviation spop 2) " fov=" fov " vision=" vision)
  print (word "Sheepesc tick-mean=" (precision s-esc-tick-mean 2) " indiv-mean=" (precision s-esc-indiv-mean 2))
  print (word "Wolves:  tick-mean=" (precision w-tick-mean 2) " indiv-mean=" (precision w-indiv-mean 2) " pop-std=" (precision standard-deviation wpop 2) " fov=" fov " vision=" vision)
  print (word "Grass:   pop-std=" (precision standard-deviation gpop 2))
end

to go
  if not any? turtles [ stop ]
  set sheep-efficiency 0
  set wolf-efficiency 0
  set sheep-escape-efficiency 0
  let num-eligible-sheep count sheep
  let available-grass grass
  set sheep-born 0
  set wolves-born 0
  set avg-grass 0
  ask sheep [
    ifelse sheep-random? [
      move-random
    ] [
      go-brain
    ]
    set energy energy - 1
    eat-grass
    death
    if energy > sheep-threshold [
      reproduce sheep-threshold
      set sheep-born sheep-born + 1
    ]
  ]
  set avg-grass safe-div avg-grass num-eligible-sheep
  set grass-eaten available-grass - grass
  set sheep-efficiency safe-div sheep-efficiency num-eligible-sheep

  let num-eligible-wolves count wolves
  set patches-with-sheep count patches with [ any? sheep-here ]
  let available-sheep count sheep
  let avail-sheep-patches count patches with [ any? sheep-here ]
  set avg-patches-with-sheep 0
  set sheep-eaten 0

  ask wolves [
    ifelse wolves-random? [
      move-random
    ] [
      go-brain
    ]
    set energy energy - 1
    catch-sheep
    death
    if energy > wolf-threshold [
      reproduce wolf-threshold
      set wolves-born wolves-born + 1
    ]
  ]
  let sheep-patches count patches with [ any? sheep-here ]
  set avg-patches-with-sheep safe-div avg-patches-with-sheep num-eligible-wolves

  set wolf-efficiency safe-div wolf-efficiency num-eligible-wolves
  set sheep-escape-efficiency safe-div sheep-escape-efficiency num-eligible-wolves

  ask patches [ grow-grass ]
  tick
end

to-report make-brain
  let b 0
  ifelse empty? brain-pool [
    (ls:create-models 1 "ANN.nlogo" [ id -> ls:hide id set b id ])
  ] [
    set b first brain-pool
    set brain-pool but-first brain-pool
  ]
  (ls:ask b [ [ ls desc ] ->
    set color-links? false
    setup ls ["sigmoid" "softmax"]
    randomize-weights
    ask first layers [
      set label item who desc
    ]
  ] layers input-descriptions )
  report b
end


to setup-brain
  set brain make-brain
  ls:set-name brain (word "Brain of " self)
  if include-null? [
    set null-brain make-brain
  ]
end

to-report in-vision-at [ agentset angle min-dist max-dist ]
  rt angle
  let result (agentset in-cone max-dist 30) with [ distance myself > min-dist ]
  lt angle
  report result
end

to go-brain
  let r random-float 1
  let i -1
  let probs sense
  while [ r > 0 and i < length probs ] [
    set i i + 1
    set r r - item i probs
  ]
  run item i outputs
  fd 1
end

to-report sense
  report apply-brain (map runresult inputs)
end

to-report apply-brain [ in ]
  ls:let inputs in
  report [ apply-reals inputs ] ls:of brain
end

to move-random
  rt one-of [-30 0 30]
  fd 1
end

to eat-grass  ;; sheep procedure
              ;; sheep eat grass, turn the patch brown
  set sheep-intake-attempts item+ grass sheep-intake-attempts 1
  set sheep-intake item+ grass sheep-intake (ifelse-value pcolor = green [ 1 ] [ 0 ])
  set avg-grass avg-grass + grass
  if pcolor = green [
    set sheep-efficiency sheep-efficiency + count patches / grass
    set pcolor brown
    set grass grass - 1
    set energy energy + sheep-gain-from-food  ;; sheep gain energy by eating
  ]
end

to reproduce [ threshold ]
  let baby-energy round (threshold * newborn-energy)
  set energy energy - baby-energy
;  ls:let child-weights map [ w -> random-normal w mut-rate ] [get-weights] ls:of brain
;  ls:let child-biases map [ b -> random-normal b mut-rate ] [get-biases] ls:of brain
  let weights [ get-layer-weights 1 ] ls:of brain
  if crossover? [
    let parent-b-weights [ [ get-layer-weights 1 ] ls:of brain ] of min-one-of other breed [ distance myself ]
    let crossover-index random length weights
    set weights (sentence (sublist weights 0 crossover-index) (sublist parent-b-weights crossover-index length parent-b-weights))
  ]
  ls:let child-weights map [ ws -> map [ w -> random-normal w mut-rate ] ws ] weights
  let child nobody
  hatch 1 [
    set energy baby-energy
    setup-brain
;    ls:ask brain [
;      set-weights child-weights
;      set-biases child-biases
;    ]
    ls:ask brain [
      set-layer-weights 1 child-weights
    ]
    rt random-float 360 fd 1
    set child self
  ]
  if include-null? [
    ls:let null-weights map [ w -> random-normal w mut-rate ] [get-weights] ls:of null-brain
    ls:let null-biases map [ b -> random-normal b mut-rate ] [get-biases] ls:of null-brain
    ask child [
      ls:ask null-brain [
        set-weights null-weights
        set-biases null-biases
      ]
    ]
  ]
  ask child [
    add-stats
  ]
end


to catch-sheep  ;; wolf procedure
  set wolf-intake-attempts item+ patches-with-sheep wolf-intake-attempts 1
  let prey one-of sheep-here
  set wolf-intake item+ patches-with-sheep wolf-intake (ifelse-value prey = nobody [ 0 ] [ 1 ])

  set avg-patches-with-sheep avg-patches-with-sheep + patches-with-sheep
  let prob-of-eating patches-with-sheep / count patches
  ifelse prey != nobody [
    set sheep-eaten sheep-eaten + 1
    set wolf-efficiency wolf-efficiency + 1 / prob-of-eating
    set energy energy + round (wolf-gain-from-food * [ energy ] of prey)
    ask prey [ kill ]
    if not any? sheep-here [
      set patches-with-sheep patches-with-sheep - 1
    ]
  ] [
    set sheep-escape-efficiency sheep-escape-efficiency + 1 / (1 - prob-of-eating)
  ]
end

to death  ;; turtle procedure
  if energy < 0 [
    kill
  ]
end

to kill
  delete-stats
  ls:set-name brain "In pool"
  set brain-pool fput brain brain-pool
  if include-null? [
    set brain-pool fput null-brain brain-pool
  ]
  die
end


to grow-grass  ;; patch procedure
               ;; countdown on brown patches: if reach 0, grow some grass
  if pcolor = brown [
    ifelse countdown <= 0
      [ set pcolor green
        set grass grass + 1
        set countdown grass-regrowth-time ]
    [ set countdown countdown - 1 ]
  ]
end

to-report towards-type [ offset model ]
  report mean map [ off ->
    item off (ls:report model [ i -> apply-reals one-hot 9 i ] (offset + off))
  ] range 3
end

to-report turn-left? [ activation ]
  report first activation and not last activation
end

to-report binary [ bool ]
  if bool [ report 1 ]
  report 0
end


to-report go-straight? [ activation ]
  report (first activation and last activation) or (not first activation and not last activation)
end

to-report turn-right? [ activation ]
  report not first activation and last activation
end

to add-stats
  change-stats 1
end

to delete-stats
  change-stats -1
end

to change-stats [ scale ]
  stop
  if is-a-sheep? self [
    set sheep-towards-grass sheep-towards-grass + scale * towards-type 0 brain
    set sheep-towards-sheep sheep-towards-sheep + scale * towards-type 3 brain
    set sheep-towards-wolves sheep-towards-wolves + scale * towards-type 6 brain
  ]
  if is-wolf? self [
    set wolves-towards-grass wolves-towards-grass + scale * towards-type 0 brain
    set wolves-towards-sheep wolves-towards-sheep + scale * towards-type 3 brain
    set wolves-towards-wolves wolves-towards-wolves + scale * towards-type 6 brain
  ]
  if include-null? [
    if is-a-sheep? self [
      set null-sheep-towards-grass null-sheep-towards-grass + scale * towards-type 0 null-brain
      set null-sheep-towards-sheep null-sheep-towards-sheep + scale * towards-type 3 null-brain
      set null-sheep-towards-wolves null-sheep-towards-wolves + scale * towards-type 6 null-brain
    ]
    if is-wolf? self [
      set null-wolves-towards-grass null-wolves-towards-grass + scale * towards-type 0 null-brain
      set null-wolves-towards-sheep null-wolves-towards-sheep + scale * towards-type 3 null-brain
      set null-wolves-towards-wolves null-wolves-towards-wolves + scale * towards-type 6 null-brain
    ]
  ]
end

to-report safe-div [ n d ]
  if d = 0 [ report 0 ]
  report n / d
end

to-report item+ [ i lst delta ]
  if length lst < i + 1 [
    set lst sentence lst n-values (i - length lst + 1) [ 0 ]
  ]
  report replace-item i lst (delta + item i lst)
end

to-report smoothed [ variable c ]
  report smoothed-val variable (runresult variable) 1 c
end

to-report smoothed-val [ name cur-value poles c ]
  foreach range poles [ i ->
    let var (word name "-" (1 + i))
    let last-value table:get-or-default smoothed-values var cur-value
    set cur-value last-value + c * (cur-value - last-value)
    table:put smoothed-values var cur-value
  ]
  report cur-value
end

to-report s-to-g
  report safe-div sheep-towards-grass count sheep
end
to-report s-to-s
  report safe-div sheep-towards-sheep count sheep
end
to-report s-to-w
  report safe-div sheep-towards-wolves count sheep
end
to-report w-to-g
  report safe-div wolves-towards-grass count wolves
end
to-report w-to-s
  report safe-div wolves-towards-sheep count wolves
end
to-report w-to-w
  report safe-div wolves-towards-wolves count wolves
end

to-report null-s-to-g
  report safe-div null-sheep-towards-grass count sheep
end
to-report null-s-to-s
  report safe-div null-sheep-towards-sheep count sheep
end
to-report null-s-to-w
  report safe-div null-sheep-towards-wolves count sheep
end
to-report null-w-to-g
  report safe-div null-wolves-towards-grass count wolves
end
to-report null-w-to-s
  report safe-div null-wolves-towards-sheep count wolves
end
to-report null-w-to-w
  report safe-div null-wolves-towards-wolves count wolves
end



to inspect-brain
  if mouse-inside? [
    ask min-one-of turtles [ distancexy mouse-xcor mouse-ycor ] [
      if subject != self [
        ask turtle-set subject [
          ls:hide brain
        ]
      ]
      set shape "default"
      watch-me
      ls:ask brain [
        set color-links? true
        ask links [ update-link-look ]
      ]
      ls:show brain
      display
    ]
  ]
end

to drag
  ifelse mouse-down? and mouse-inside? [
    if not is-turtle? drag-target [
      set drag-target min-one-of turtles [ distancexy mouse-xcor mouse-ycor ]
    ]
    ask drag-target [
      facexy mouse-xcor mouse-ycor
      ask drag-target [ setxy mouse-xcor mouse-ycor ]
    ]
  ] [
    set drag-target nobody
  ]
  display
end


; Copyright 1997 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
710
10
1228
529
-1
-1
10.0
1
14
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

SLIDER
0
10
175
43
initial-number-sheep
initial-number-sheep
0
250
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
220
175
253
sheep-gain-from-food
sheep-gain-from-food
0.0
50.0
5.0
1.0
1
NIL
HORIZONTAL

SLIDER
175
10
350
43
initial-number-wolves
initial-number-wolves
0
250
50.0
1
1
NIL
HORIZONTAL

SLIDER
0
185
175
218
grass-regrowth-time
grass-regrowth-time
0
100
30.0
1
1
NIL
HORIZONTAL

BUTTON
0
150
69
183
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
70
150
137
183
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

PLOT
355
270
705
495
populations
time
pop.
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"sheep" 1.0 0 -13345367 true "" "plot count sheep"
"wolves" 1.0 0 -2674135 true "" "plot count wolves"
"grass / 4" 1.0 0 -10899396 true "" "plot grass / 4"

SLIDER
0
80
175
113
vision
vision
0
5
3.0
0.5
1
NIL
HORIZONTAL

SLIDER
175
80
350
113
fov
fov
30
360
150.0
60
1
NIL
HORIZONTAL

BUTTON
355
10
415
43
inspect
inspect-brain
NIL
1
T
OBSERVER
NIL
I
NIL
NIL
1

BUTTON
415
10
535
43
reset-perspective
ask turtles [ stop-inspecting self ]\nreset-perspective\nstop-inspecting-dead-agents\nls:hide ls:models\nls:ask ls:models [ set color-links? false ]\nask sheep [ set shape \"sheep\" ]\nask wolves [ set shape \"wolf\" ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
0
385
350
580
sheep-reactions
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"towards-grass" 1.0 0 -10899396 true "" "plot s-to-g"
"towards-sheep" 1.0 0 -13345367 true "" "plot s-to-s"
"towards-wolves" 1.0 0 -2674135 true "" "plot s-to-w"

PLOT
0
580
350
775
wolf-reactions
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"towards-grass" 1.0 0 -10899396 true "" "plot w-to-g"
"towards-sheep" 1.0 0 -13345367 true "" "plot w-to-s"
"towards-wolves" 1.0 0 -2674135 true "" "plot w-to-w"

MONITOR
640
355
697
400
sheep
count sheep
17
1
11

MONITOR
640
400
697
445
wolves
count wolves
17
1
11

SWITCH
175
45
350
78
include-null?
include-null?
1
1
-1000

INPUTBOX
0
325
80
385
mut-rate
0.1
1
0
Number

BUTTON
535
10
650
43
update-subject
ask turtle-set subject [ __ignore sense ]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
650
10
705
43
drag
if mouse-down? and mouse-inside? [\n  ask min-one-of turtles [ distancexy mouse-xcor mouse-ycor ] [\n    setxy mouse-xcor mouse-ycor\n  ]\n]\ndisplay
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
355
45
705
270
smoothed efficiency
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"sheep" 1.0 0 -13345367 true "" "plot smoothed-val \"seff\" sheep-efficiency 6 0.1"
"wolf" 1.0 0 -2674135 true "" "plot smoothed-val \"weff\" wolf-efficiency 6 0.1"
"escape" 1.0 0 -11221820 true "" "plot smoothed-val \"escape\" sheep-escape-efficiency 6 0.1"

SWITCH
0
290
175
323
sheep-random?
sheep-random?
1
1
-1000

SWITCH
175
290
350
323
wolves-random?
wolves-random?
1
1
-1000

MONITOR
640
125
697
170
sheep
table:get smoothed-values \"seff-6\"
3
1
11

MONITOR
640
215
697
260
wolves
table:get smoothed-values \"weff-6\"
3
1
11

SLIDER
0
255
175
288
sheep-threshold
sheep-threshold
0
200
70.0
10
1
NIL
HORIZONTAL

SLIDER
175
255
350
288
wolf-threshold
wolf-threshold
0
200
70.0
10
1
NIL
HORIZONTAL

BUTTON
195
150
307
183
NIL
sample-effs\n
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
0
45
175
78
initial-grass-density
initial-grass-density
0
1
0.35
0.05
1
NIL
HORIZONTAL

SLIDER
175
185
350
218
newborn-energy
newborn-energy
0
1
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
175
220
350
253
wolf-gain-from-food
wolf-gain-from-food
0
1
0.7
0.1
1
NIL
HORIZONTAL

MONITOR
640
170
697
215
escape
table:get smoothed-values \"escape-6\"
3
1
11

SWITCH
175
325
350
358
crossover?
crossover?
0
1
-1000

SLIDER
0
115
175
148
granularity
granularity
1
vision
1.0
1
1
NIL
HORIZONTAL

SLIDER
175
115
350
148
hidden-nodes
hidden-nodes
1
20
6.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model explores the stability of predator-prey ecosystems. Such a system is called unstable if it tends to result in extinction for one or more species involved.  In contrast, a system is stable if it tends to maintain itself over time, despite fluctuations in population sizes.

## HOW IT WORKS

There are two main variations to this model.

In the first variation, wolves and sheep wander randomly around the landscape, while the wolves look for sheep to prey on. Each step costs the wolves energy, and they must eat sheep in order to replenish their energy - when they run out of energy they die. To allow the population to continue, each wolf or sheep has a fixed probability of reproducing at each time step. This variation produces interesting population dynamics, but is ultimately unstable.

The second variation includes grass (green) in addition to wolves and sheep. The behavior of the wolves is identical to the first variation, however this time the sheep must eat grass in order to maintain their energy - when they run out of energy they die. Once grass is eaten it will only regrow after a fixed amount of time. This variation is more complex than the first, but it is generally stable.

The construction of this model is described in two papers by Wilensky & Reisman referenced below.

## HOW TO USE IT

1. Set the GRASS? switch to TRUE to include grass in the model, or to FALSE to only include wolves (red) and sheep (white).
2. Adjust the slider parameters (see below), or use the default settings.
3. Press the SETUP button.
4. Press the GO button to begin the simulation.
5. Look at the monitors to see the current population sizes
6. Look at the POPULATIONS plot to watch the populations fluctuate over time

Parameters:
INITIAL-NUMBER-SHEEP: The initial size of sheep population
INITIAL-NUMBER-WOLVES: The initial size of wolf population
SHEEP-GAIN-FROM-FOOD: The amount of energy sheep get for every grass patch eaten
WOLF-GAIN-FROM-FOOD: The amount of energy wolves get for every sheep eaten
SHEEP-REPRODUCE: The probability of a sheep reproducing at each time step
WOLF-REPRODUCE: The probability of a wolf reproducing at each time step
GRASS?: Whether or not to include grass in the model
GRASS-REGROWTH-TIME: How long it takes for grass to regrow once it is eaten
SHOW-ENERGY?: Whether or not to show the energy of each animal as a number

Notes:
- one unit of energy is deducted for every step a wolf takes
- when grass is included, one unit of energy is deducted for every step a sheep takes

## THINGS TO NOTICE

When grass is not included, watch as the sheep and wolf populations fluctuate. Notice that increases and decreases in the sizes of each population are related. In what way are they related? What eventually happens?

Once grass is added, notice the green line added to the population plot representing fluctuations in the amount of grass. How do the sizes of the three populations appear to relate now? What is the explanation for this?

Why do you suppose that some variations of the model might be stable while others are not?

## THINGS TO TRY

Try adjusting the parameters under various settings. How sensitive is the stability of the model to the particular parameters?

Can you find any parameters that generate a stable ecosystem that includes only wolves and sheep?

Try setting GRASS? to TRUE, but setting INITIAL-NUMBER-WOLVES to 0. This gives a stable ecosystem with only sheep and grass. Why might this be stable while the variation with only sheep and wolves is not?

Notice that under stable settings, the populations tend to fluctuate at a predictable pace. Can you find any parameters that will speed this up or slow it down?

Try changing the reproduction rules -- for example, what would happen if reproduction depended on energy rather than being determined by a fixed probability?

## EXTENDING THE MODEL

There are a number ways to alter the model so that it will be stable with only wolves and sheep (no grass). Some will require new elements to be coded in or existing behaviors to be changed. Can you develop such a version?

## NETLOGO FEATURES

Note the use of breeds to model two different kinds of "turtles": wolves and sheep. Note the use of patches to model grass.

Note use of the ONE-OF agentset reporter to select a random sheep to be eaten by a wolf.

## RELATED MODELS

Look at Rabbits Grass Weeds for another model of interacting populations with different rules.

## CREDITS AND REFERENCES

Wilensky, U. & Reisman, K. (1999). Connected Science: Learning Biology through Constructing and Testing Computational Theories -- an Embodied Modeling Approach. International Journal of Complex Systems, M. 234, pp. 1 - 12. (This model is a slightly extended version of the model described in the paper.)

Wilensky, U. & Reisman, K. (2006). Thinking like a Wolf, a Sheep or a Firefly: Learning Biology through Constructing and Testing Computational Theories -- an Embodied Modeling Approach. Cognition & Instruction, 24(2), pp. 171-209. http://ccl.northwestern.edu/papers/wolfsheep.pdf


## HOW TO CITE

If you mention this model in a publication, we ask that you include these citations for the model itself and for the NetLogo software:

* Wilensky, U. (1997).  NetLogo Wolf Sheep Predation model.  http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1997 Uri Wilensky.

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2000.
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
NetLogo 6.2.2
@#$#@#$#@
setup
set grass? true
repeat 75 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="with-nulls" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <exitCondition>not any? wolves or not any? sheep</exitCondition>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>s-to-g</metric>
    <metric>s-to-s</metric>
    <metric>s-to-w</metric>
    <metric>w-to-g</metric>
    <metric>w-to-s</metric>
    <metric>w-to-w</metric>
    <metric>null-s-to-g</metric>
    <metric>null-s-to-s</metric>
    <metric>null-s-to-w</metric>
    <metric>null-w-to-g</metric>
    <metric>null-w-to-s</metric>
    <metric>null-w-to-w</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduce-%">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="with-nulls-big" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <exitCondition>not any? wolves or not any? sheep</exitCondition>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>s-to-g</metric>
    <metric>s-to-s</metric>
    <metric>s-to-w</metric>
    <metric>w-to-g</metric>
    <metric>w-to-s</metric>
    <metric>w-to-w</metric>
    <metric>null-s-to-g</metric>
    <metric>null-s-to-s</metric>
    <metric>null-s-to-w</metric>
    <metric>null-w-to-g</metric>
    <metric>null-w-to-s</metric>
    <metric>null-w-to-w</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduce-%">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="small" repetitions="8" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>s-to-g</metric>
    <metric>s-to-s</metric>
    <metric>s-to-w</metric>
    <metric>w-to-g</metric>
    <metric>w-to-s</metric>
    <metric>w-to-w</metric>
    <metric>null-s-to-g</metric>
    <metric>null-s-to-s</metric>
    <metric>null-s-to-w</metric>
    <metric>null-w-to-g</metric>
    <metric>null-w-to-s</metric>
    <metric>null-w-to-w</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="new-small" repetitions="8" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>s-to-g</metric>
    <metric>s-to-s</metric>
    <metric>s-to-w</metric>
    <metric>w-to-g</metric>
    <metric>w-to-s</metric>
    <metric>w-to-w</metric>
    <metric>null-s-to-g</metric>
    <metric>null-s-to-s</metric>
    <metric>null-s-to-w</metric>
    <metric>null-w-to-g</metric>
    <metric>null-w-to-s</metric>
    <metric>null-w-to-w</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="bigger" repetitions="32" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>s-to-g</metric>
    <metric>s-to-s</metric>
    <metric>s-to-w</metric>
    <metric>w-to-g</metric>
    <metric>w-to-s</metric>
    <metric>w-to-w</metric>
    <metric>null-s-to-g</metric>
    <metric>null-s-to-s</metric>
    <metric>null-s-to-w</metric>
    <metric>null-w-to-g</metric>
    <metric>null-w-to-s</metric>
    <metric>null-w-to-w</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
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
1
@#$#@#$#@
