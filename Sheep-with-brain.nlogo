extensions [ ls table csv ]


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

  sheep-inputs
  sheep-input-descriptions
  wolf-inputs
  wolf-input-descriptions

  outputs
  brain-pool
  sheep-layers
  wolf-layers

  sheep-reactions
  wolf-reactions

  drag-target

  ; only used with single-brain? enabled
  sheep-brain
  wolf-brain

  sheep-probs
  wolf-probs

  sheep-eat-attempts
  wolf-eat-attempts

  sheep-cert
  sheep-lcert
  sheep-scert
  sheep-rcert
  sheep-lnum
  sheep-snum
  sheep-rnum
  sheep-lprob
  sheep-sprob
  sheep-rprob

  wolf-cert
  wolf-lcert
  wolf-scert
  wolf-rcert
  wolf-lnum
  wolf-snum
  wolf-rnum
  wolf-lprob
  wolf-sprob
  wolf-rprob
]

;; Sheep and wolves are both breeds of turtle.
breed [sheep a-sheep]  ;; sheep is its own plural, so we use "a-sheep" as the singular.
breed [wolves wolf]
turtles-own [
  energy
  brain
  null-brain
  probs
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


  set sheep-probs []
  set wolf-probs []

  set sheep-inputs input-pairs sheep-see-grass? sheep-see-sheep? sheep-see-wolves?
  set sheep-input-descriptions map first sheep-inputs
  set sheep-inputs map last sheep-inputs

  set wolf-inputs input-pairs wolves-see-grass? wolves-see-sheep? wolves-see-wolves?
  set wolf-input-descriptions map first wolf-inputs
  set wolf-inputs map last wolf-inputs

  set outputs (list
    [-> lt 30]
    [->]
    [-> rt 30]
  )

  if bs-save-weights? and behaviorspace-run-number > 0 [
    file-open (word behaviorspace-experiment-name "_weights_" behaviorspace-run-number ".csv")
    file-print (csv:to-row (list "run-number" "tick" "breed" "agent" "event" "weights"))
  ]

  set sheep-reactions map [ i -> map [ o -> 0 ] outputs ] sheep-inputs
  set wolf-reactions map [ i -> map [ o -> 0 ] outputs ] wolf-inputs

  set sheep-layers (sentence (length sheep-inputs) hidden-nodes (length outputs))
  set wolf-layers (sentence (length wolf-inputs) hidden-nodes (length outputs))

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
    record-weights -1 "born"
  ]

  set grass count patches with [pcolor = green]

  set drag-target nobody

  reset-ticks
end

to-report input-pairs [ grass? sheep? wolves? ]
  let min-vision-dist 0
  let seg-size (vision - min-vision-dist) / granularity
  report lput (list "bias" [ -> 1 ]) (reduce sentence
;    (map [ min-dist ->
;      reduce sentence map [ angle ->
;        (sentence
;          (ifelse-value grass? [ (list grass-input angle min-dist seg-size) ] [ [] ])
;          (ifelse-value sheep? [ (list sheep-input angle min-dist seg-size) ] [ [] ])
;          (ifelse-value wolves? [ (list wolf-input angle min-dist seg-size) ] [ [] ])
;        )
;      ] (range (fov / -2 + 15) (fov / 2 - 15 + 1) 30)
;    ] (range min-vision-dist vision seg-size))
    (map [ angle ->
      reduce sentence map [ min-dist ->
        (sentence
          (ifelse-value grass? [ (list grass-input angle min-dist seg-size) ] [ [] ])
          (ifelse-value sheep? [ (list sheep-input angle min-dist seg-size) ] [ [] ])
          (ifelse-value wolves? [ (list wolf-input angle min-dist seg-size) ] [ [] ])
        )
      ] (range min-vision-dist vision seg-size)
    ] (range (fov / -2 + 15) (fov / 2 - 15 + 1) 30))
  )
end

to-report grass-input [ angle min-dist seg-size ]
  report (list
    (list "g" (min-dist + seg-size) angle)
;    (word "g " (precision (min-dist + seg-size) 1) " " angle)
    [ -> binary
      any? (in-vision-at patches angle min-dist (min-dist +  seg-size))
      with [ pcolor = green ]
    ]
  )
end

to-report sheep-input [ angle min-dist seg-size ]
  report (list
    (list "s" (min-dist + seg-size) angle)
;    (word "s " (precision (min-dist + seg-size) 1) " " angle)
    [ -> binary
      any? other (in-vision-at sheep angle min-dist (min-dist + seg-size))
    ]
  )
end

to-report wolf-input [ angle min-dist seg-size ]
  report (list
    (list "w" (min-dist + seg-size) angle)
;    (word "w " (precision (min-dist + seg-size) 1) " " angle)
    [ -> binary
      any? other (in-vision-at wolves angle min-dist (min-dist + seg-size))
    ]
  )
end

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

to go-n [ n ]
  let wsum-sheep-efficiency 0
  let wsum-sheep-escape-efficiency 0
  let wsum-wolf-efficiency 0
  set sheep-eat-attempts 0
  set wolf-eat-attempts 0

  set sheep-cert 0
  set sheep-lcert 0
  set sheep-scert 0
  set sheep-rcert 0
  set sheep-lnum 0
  set sheep-snum 0
  set sheep-rnum 0
  set sheep-lprob 0
  set sheep-sprob 0
  set sheep-rprob 0

  set wolf-cert 0
  set wolf-lcert 0
  set wolf-scert 0
  set wolf-rcert 0
  set wolf-lnum 0
  set wolf-snum 0
  set wolf-rnum 0
  set wolf-lprob 0
  set wolf-sprob 0
  set wolf-rprob 0

  repeat n [
    let num-sheep count sheep
    let num-wolves count wolves

    go

    set wsum-sheep-efficiency wsum-sheep-efficiency + sheep-efficiency * num-sheep
    set wsum-sheep-escape-efficiency wsum-sheep-escape-efficiency + sheep-escape-efficiency * num-wolves
    set wsum-wolf-efficiency wsum-wolf-efficiency + wolf-efficiency * num-wolves

    set sheep-eat-attempts sheep-eat-attempts + num-sheep
    set wolf-eat-attempts wolf-eat-attempts + num-wolves

    set sheep-cert sheep-cert + sheep-certainty

    let sheep-l filter-action sheep-probs 0
    set sheep-lcert sheep-lcert + sum sheep-l
    set sheep-lnum sheep-lnum + length sheep-l

    let sheep-s filter-action sheep-probs 1
    set sheep-scert sheep-scert + sum sheep-s
    set sheep-snum sheep-snum + length sheep-s

    let sheep-r filter-action sheep-probs 2
    set sheep-rcert sheep-rcert + sum sheep-r
    set sheep-rnum sheep-rnum + length sheep-r

    set sheep-lprob sheep-lprob + sum action-probs sheep-probs 0
    set sheep-sprob sheep-sprob + sum action-probs sheep-probs 1
    set sheep-rprob sheep-rprob + sum action-probs sheep-probs 2

    set wolf-cert wolf-cert + wolf-certainty

    let wolf-l filter-action wolf-probs 0
    set wolf-lcert wolf-lcert + sum wolf-l
    set wolf-lnum wolf-lnum + length wolf-l

    let wolf-s filter-action wolf-probs 1
    set wolf-scert wolf-scert + sum wolf-s
    set wolf-snum wolf-snum + length wolf-s

    let wolf-r filter-action wolf-probs 2
    set wolf-rcert wolf-rcert + sum wolf-r
    set wolf-rnum wolf-rnum + length wolf-r

    set wolf-lprob wolf-lprob + sum action-probs wolf-probs 0
    set wolf-sprob wolf-sprob + sum action-probs wolf-probs 1
    set wolf-rprob wolf-rprob + sum action-probs wolf-probs 2
  ]

  set sheep-efficiency safe-div wsum-sheep-efficiency sheep-eat-attempts
  set sheep-escape-efficiency safe-div wsum-sheep-escape-efficiency wolf-eat-attempts
  set wolf-efficiency safe-div wsum-wolf-efficiency wolf-eat-attempts

  set sheep-cert safe-div sheep-cert sheep-eat-attempts
  set sheep-lcert safe-div sheep-lcert sheep-lnum
  set sheep-scert safe-div sheep-scert sheep-snum
  set sheep-rcert safe-div sheep-rcert sheep-rnum
  set sheep-lprob safe-div sheep-lprob sheep-eat-attempts
  set sheep-sprob safe-div sheep-sprob sheep-eat-attempts
  set sheep-rprob safe-div sheep-rprob sheep-eat-attempts

  set wolf-cert safe-div wolf-cert wolf-eat-attempts
  set wolf-lcert safe-div wolf-lcert wolf-lnum
  set wolf-scert safe-div wolf-scert wolf-snum
  set wolf-rcert safe-div wolf-rcert wolf-rnum
  set wolf-lprob safe-div wolf-lprob wolf-eat-attempts
  set wolf-sprob safe-div wolf-sprob wolf-eat-attempts
  set wolf-rprob safe-div wolf-rprob wolf-eat-attempts
end

to go
  if not any? turtles [ stop ]
  set sheep-probs []
  set wolf-probs []
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
  ; Allow weight initialization to be based off mut-rate
;  let iw init-weights
;  let mr-ix position "mut-rate" iw
;  if is-number? mr-ix [
;    set iw insert-item mr-ix (remove "mut-rate" iw) (word mut-rate)
;  ]
;
;  ls:let iw iw
  ls:let ls layers
  ls:let desc map stringify-desc input-descriptions
  ls:let mut-rate mut-rate
  ls:ask b [
    set color-links? false
    setup ls ["sigmoid" "softmax"]
;    randomize-weights
    ask links [ set-weight random-normal 0 mut-rate ]
    ask first layers [
      set label item who desc
    ]
    (foreach (sort last layers) [ "left" "straight" "right" ] [ [ node l ] ->
      ask node [ set label l ]
    ])
    ask turtles with [ label = "bias" ] [ ask my-out-links [ set-weight 0 ] ]
  ]
  report b
end

to-report stringify-desc [ d ]
  ifelse is-string? d [
    report d
  ] [
    report (word (item 0 d) " " (precision (item 1 d) 1) " " (precision (item 2 d) 1))
  ]
end

to-report layers
  report ifelse-value breed = sheep [ sheep-layers ] [ wolf-layers ]
end

to-report input-descriptions
  report ifelse-value breed = sheep [ sheep-input-descriptions ] [ wolf-input-descriptions ]
end

to-report inputs
  report ifelse-value breed = sheep [ sheep-inputs ] [ wolf-inputs ]
end


to setup-brain
  ifelse single-brain? [
    ifelse breed = sheep [
      if empty? ls:models or ls:name-of sheep-brain != "sheep brain" [
        set sheep-brain make-brain
        ls:set-name sheep-brain "sheep brain"
      ]
      set brain sheep-brain
    ] [
      if empty? ls:models or ls:name-of wolf-brain != "wolf brain" [
        set wolf-brain make-brain
        ls:set-name wolf-brain "wolf brain"
      ]
      set brain wolf-brain
    ]
  ] [
    set brain make-brain
    ls:set-name brain (word "Brain of " self)
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
  set probs sense
  ifelse breed = sheep [
    set sheep-probs lput probs sheep-probs
  ] [
    set wolf-probs lput probs wolf-probs
  ]
  while [ r > 0 and i < length probs ] [
    set i i + 1
    set r r - item i probs
  ]
;  set i position (max probs) probs
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
  if crossover? and any? other breed [
    let parent-b-weights [ [ get-layer-weights 1 ] ls:of brain ] of min-one-of other breed [ distance myself ]
    let crossover-index random length weights
    set weights (sentence (sublist weights 0 crossover-index) (sublist parent-b-weights crossover-index length parent-b-weights))
  ]
  ls:let child-weights map [ ws -> map [ w -> random-normal w mut-rate ] ws ] weights
;  ls:let child-weights weights
  ls:let mr mut-rate
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
;      (foreach (sort item 1 layers) child-weights [ [ node ws ] ->
;        ask node [
;          (foreach (sort my-links) ws [ [ l w ] ->
;            ask l [ set-weight w + mr * random-xavier ]
;          ])
;        ]
;      ])
    ]
    rt random-float 360 fd 1
    set child self
    record-weights ticks (word "born " [ who ] of myself)
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
    ask prey [
      record-weights ticks (word "eaten " ([ who ] of myself))
      kill
    ]
    if not any? sheep-here [
      set patches-with-sheep patches-with-sheep - 1
    ]
  ] [
    set sheep-escape-efficiency sheep-escape-efficiency + 1 / (1 - prob-of-eating)
  ]
end

to death  ;; turtle procedure
  if energy < 0 [
    record-weights ticks "starved"
    kill
  ]
end

to kill
  delete-stats
  if not single-brain? [
    ls:set-name brain "In pool"
    set brain-pool fput brain brain-pool
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

to-report action-probs [ prob-list action ]
  report map [ ps -> item action ps ] prob-list
end

to-report action-prob [ prob-list action ]
  if empty? prob-list [
    report 0
  ]
  report mean action-probs prob-list action
end

to-report filter-action [ prob-list action ]
  report map [ ps -> item action ps ] filter [ ps -> max ps = item action ps ] prob-list
end

to-report action-certainty [ prob-list action ]
  let selected map [ ps -> item action ps ] filter [ ps -> max ps = item action ps ] prob-list
  ifelse empty? selected [
    report 0
  ] [
    report mean selected
  ]
end


to-report sheep-certainty
  if empty? sheep-probs [
    report 0
  ]
  report mean map max sheep-probs
end

to-report sheep-left-prob
  report action-prob sheep-probs 0
end

to-report sheep-straight-prob
  report action-prob sheep-probs 1
end

to-report sheep-right-prob
  report action-prob sheep-probs 2
end

to-report sheep-left-certainty
  report action-certainty sheep-probs 0
end

to-report sheep-straight-certainty
  report action-certainty sheep-probs 1
end

to-report sheep-right-certainty
  report action-certainty sheep-probs 2
end

to-report wolf-certainty
  if empty? wolf-probs [
    report 0
  ]
  report mean map max wolf-probs
end

to-report wolf-left-prob
  report action-prob wolf-probs 0
end

to-report wolf-straight-prob
  report action-prob wolf-probs 1
end

to-report wolf-right-prob
  report action-prob wolf-probs 2
end

to-report wolf-left-certainty
  report action-certainty wolf-probs 0
end

to-report wolf-straight-certainty
  report action-certainty wolf-probs 1
end

to-report wolf-right-certainty
  report action-certainty wolf-probs 2
end

to plot-nz [ value ]
  if value != 0 [
    plotxy ticks value
  ]
end

to-report binary [ bool ]
  if bool [ report 1 ]
  report 0
end

to-report serialize-event [ t event ]
  let weights [ get-weights ] ls:of brain
  let formatted-weights (word "[" reduce [ [accum x] -> (word accum "," x) ] map [ x -> precision x 3 ] weights "]")
  report csv:to-row (list behaviorspace-run-number t breed who event formatted-weights)
end

to record-weights [ t event ]
  if bs-save-weights? and behaviorspace-run-number > 0 [
    file-print serialize-event t event
  ]
end

to add-stats
  change-stats  1
end

to delete-stats
  change-stats -1
end

to-report get-reactions
  report map [ i ->
    (ls:report brain [ j -> apply-reals lput 1 one-hot (count first layers - 1) j ] i)
  ] range (length inputs)

end

to change-stats [ scale ]
  if not track-reactions? [ stop ]
  if is-a-sheep? self [
    set sheep-reactions (map [ [ totals new ] ->
      (map [ [ t n ] -> t + scale * n ] totals new)
    ] sheep-reactions get-reactions)
  ]
  if is-wolf? self [
    set wolf-reactions (map [ [ totals new ] ->
      (map [ [ t n ] -> t + scale * n ] totals new)
    ] wolf-reactions get-reactions)
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
  ifelse mouse-down? [
    if mouse-inside? [
      if not is-turtle? drag-target [
        set drag-target min-one-of turtles [ distancexy mouse-xcor mouse-ycor ]
      ]
      ask drag-target [ setxy mouse-xcor mouse-ycor ]
    ]
  ] [
    set drag-target nobody
  ]
  display
end

to face-mouse
  ask subject [ facexy mouse-xcor mouse-ycor ]
end

to setup-reaction-plot [ descriptions ]
  let i 0
  let n length descriptions
  foreach descriptions [ d ->
    create-temporary-plot-pen stringify-desc d
    ifelse is-string? d [
      set-plot-pen-color black
    ] [
      let species item 0 d
      let dist item 1 d
      let angle item 2 d
      let r 0
      let g 0
      let b 0
;      (ifelse
;        first d = "s" [ set b 255 ]
;        first d = "w" [ set r 255 ]
;        [ set b 255 set r 255]
;      )
;      set r r * (item 1 d) / vision
;      set b b * (item 1 d) / vision
;      set g 255 * ((fov) / 2 + last d) / fov
      (ifelse
        species = "s" [
          set b 128 + 127 * dist / vision
          ifelse angle < 0 [
            set g 32 + 128 * (0 - angle) / (fov / 2 - 15)
          ] [
            set r 32 + 128 * angle / (fov / 2 - 15)
          ]
        ]
        species = "w" [
          set r 128 + 127 * dist / vision
          ifelse angle < 0 [
            set b 32 + 128 * (0 - angle) / (fov / 2 - 15)
          ] [
            set g 32 + 128 * angle / (fov / 2 - 15)
          ]
        ]
        species = "g" [
          set g 128 + 127 * dist / vision
          (ifelse angle < 0 [
            set r 32 + 128 * (0 - angle) / (fov / 2 - 15)
          ] angle > 0 [
            set b 32 + 128 * angle / (fov / 2 - 15)
          ])
        ]
      )
      set-plot-pen-color (list r g b)
;      let h (ifelse-value
;        species = "w" [ 0 ]
;        species = "g" [ 120 ]
;        species = "s" [ 240 ]
;      ) + angle
;      let s 100
;      let b dist / vision * 50 + 50
;      set-plot-pen-color hsb h s b
      set i i + 1
    ]
  ]
end

to update-reaction-plot [ descriptions reactions output agentset ]
  if not track-reactions? [ stop ]
  (foreach descriptions reactions [ [ d r ] ->
    set-current-plot-pen stringify-desc d
    plot (item output r) / count agentset
  ])
end


to update-subject
  display
  cd
  ask turtle-set subject [
    set probs sense
    hatch 1 [
      set color yellow
      set shape "dot"
      set size 0.1
      pd
      foreach (range (fov / -2) (fov / 2 + 1) 30) [ angle ->
        rt angle
        fd vision
        bk vision
        rt 0 - angle
      ]
      foreach (range 1 (granularity + 1)) [ g ->
        let r vision * g / granularity
        foreach (range (fov / -2) (fov / 2 + 1) 0.1) [ angle ->
          pu
          rt angle
          fd r
          stamp
          bk r
          rt 0 - angle
        ]
      ]
      die
    ]
  ]
  display
end

to-report mean-vec [ vecs ]
  report n-values length first vecs [ i -> mean map [ v -> item i v ] vecs ]
end

to-report vec-dist [ xs ys ]
  report sqrt sum (map [ [x y] -> (x - y) ^ 2 ] xs ys)
end

to-report heterogeneity [ vecs ]
  let m mean-vec vecs
  report mean map [ v -> vec-dist v m ] vecs
end

to-report create-sample-model
  let model 0
  (ls:create-models 1 "Sheep-with-brain.nlogo" [ id ->
    ls:assign id initial-number-sheep initial-number-sheep
    ls:assign id initial-number-wolves initial-number-wolves
    ls:assign id initial-grass-density initial-grass-density
    ls:assign id single-brain? true
    ls:assign id vision vision
    ls:assign id fov fov
    ls:assign id granularity granularity
    ls:assign id hidden-nodes hidden-nodes
    ls:assign id sheep-see-grass? sheep-see-grass?
    ls:assign id sheep-see-wolves? sheep-see-wolves?
    ls:assign id sheep-see-sheep? sheep-see-sheep?
    ls:assign id wolves-see-grass? wolves-see-grass?
    ls:assign id wolves-see-wolves? wolves-see-wolves?
    ls:assign id wolves-see-sheep? wolves-see-sheep?
    ls:assign id grass-regrowth-time grass-regrowth-time
    ls:assign id newborn-energy newborn-energy
    ls:assign id sheep-gain-from-food sheep-gain-from-food
    ls:assign id wolf-gain-from-food wolf-gain-from-food
    ls:assign id sheep-threshold sheep-threshold
    ls:assign id wolf-threshold wolf-threshold
    ls:assign id sheep-random? sheep-random?
    ls:assign id wolves-random? wolves-random?
    ls:assign id mut-rate 0
    ls:assign id crossover? false
    ls:assign id track-reactions? false
    ls:assign id bs-save-weights? false
    set model id
  ])
  report model
end

to-report sample-eff [ n ]
  let model create-sample-model

  ls:ask model [ setup ]

  ifelse breed = sheep [
    let weights [ get-weights ] ls:of brain
    (ls:ask model [ ws ->
      (ls:ask sheep-brain [ w -> set-weights w ] ws)
    ] weights)
  ] [
    let weights [ get-weights ] ls:of brain
    (ls:ask model [ ws ->
      (ls:ask wolf-brain [ ws -> set-weights ws ] ws)
    ])
  ]
  let weff []
  let seff []
  let wpop []
  let spop []
  let gpop []

  let seff-escape []

  repeat n [
    set spop lput [ count sheep ] ls:of model spop
    set wpop lput [ count wolves ] ls:of model wpop
    set gpop lput [ grass ] ls:of model gpop
    ls:ask model [ go ]
    set weff lput [ wolf-efficiency ] ls:of model weff
    set seff lput [ sheep-efficiency ] ls:of model seff
    set seff-escape lput [ sheep-escape-efficiency ] ls:of model seff-escape
  ]
  let w-indiv-mean (sum (map * weff wpop)) / (sum wpop)
  let s-indiv-mean (sum (map * seff spop)) / (sum spop)
  let s-esc-indiv-mean safe-div (sum (map * seff-escape spop)) (sum spop)

  ls:close model

  report (list s-indiv-mean s-esc-indiv-mean w-indiv-mean)
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
325
175
358
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
290
175
323
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
255
80
288
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
80
255
150
288
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
315
705
540
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
210.0
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
cd\nask turtles [ stop-inspecting self ]\nreset-perspective\nstop-inspecting-dead-agents\nls:hide ls:models\nls:ask ls:models [ set color-links? false ]\nask sheep [ set shape \"sheep\" ]\nask wolves [ set shape \"wolf\" ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
640
400
697
445
sheep
count sheep
17
1
11

MONITOR
640
445
697
490
wolves
count wolves
17
1
11

INPUTBOX
0
430
80
490
mut-rate
10.0
1
0
Number

BUTTON
535
10
650
43
NIL
update-subject
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
drag
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
90
705
315
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
"sheep" 1.0 0 -13345367 true "" "if ticks > 0 [ plotxy ticks smoothed-val \"seff\" sheep-efficiency 6 0.1 ]"
"wolf" 1.0 0 -2674135 true "" "if ticks > 0 [ plotxy ticks smoothed-val \"weff\" wolf-efficiency 6 0.1 ]"
"escape" 1.0 0 -11221820 true "" "if ticks > 0 [ plotxy ticks smoothed-val \"escape\" sheep-escape-efficiency 6 0.1 ]"

SWITCH
0
395
175
428
sheep-random?
sheep-random?
1
1
-1000

SWITCH
175
395
350
428
wolves-random?
wolves-random?
0
1
-1000

MONITOR
640
170
697
215
sheep
table:get smoothed-values \"seff-6\"
3
1
11

MONITOR
640
260
697
305
wolves
table:get smoothed-values \"weff-6\"
3
1
11

SLIDER
0
360
175
393
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
360
350
393
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
175
255
250
288
sample
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
290
350
323
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
325
350
358
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
215
697
260
escape
table:get smoothed-values \"escape-6\"
3
1
11

SWITCH
175
430
350
463
crossover?
crossover?
1
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
2.0
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

SWITCH
0
150
175
183
sheep-see-grass?
sheep-see-grass?
0
1
-1000

SWITCH
0
185
175
218
sheep-see-wolves?
sheep-see-wolves?
0
1
-1000

SWITCH
0
220
175
253
sheep-see-sheep?
sheep-see-sheep?
0
1
-1000

SWITCH
175
150
350
183
wolves-see-grass?
wolves-see-grass?
0
1
-1000

SWITCH
175
185
350
218
wolves-see-wolves?
wolves-see-wolves?
0
1
-1000

SWITCH
175
220
350
253
wolves-see-sheep?
wolves-see-sheep?
0
1
-1000

PLOT
0
540
270
905
sheep-left
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"setup-reaction-plot sheep-input-descriptions" "update-reaction-plot sheep-input-descriptions sheep-reactions 0 sheep"
PENS

PLOT
270
540
540
905
sheep-straight
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"setup-reaction-plot sheep-input-descriptions" "update-reaction-plot sheep-input-descriptions sheep-reactions 1 sheep"
PENS

PLOT
540
540
810
905
sheep-right
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"setup-reaction-plot sheep-input-descriptions" "update-reaction-plot sheep-input-descriptions sheep-reactions 2 sheep"
PENS

PLOT
810
540
1080
905
wolves-left
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"setup-reaction-plot wolf-input-descriptions" "update-reaction-plot wolf-input-descriptions wolf-reactions 0 wolves"
PENS

PLOT
1080
540
1350
905
wolves-straight
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"setup-reaction-plot wolf-input-descriptions" "update-reaction-plot wolf-input-descriptions wolf-reactions 1 wolves"
PENS

PLOT
1350
540
1620
905
wolves-right
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"setup-reaction-plot wolf-input-descriptions" "update-reaction-plot wolf-input-descriptions wolf-reactions 2 wolves"
PENS

SWITCH
0
490
172
523
track-reactions?
track-reactions?
1
1
-1000

SWITCH
175
490
350
523
bs-save-weights?
bs-save-weights?
1
1
-1000

SWITCH
175
45
350
78
single-brain?
single-brain?
1
1
-1000

BUTTON
535
45
645
78
NIL
face-mouse
NIL
1
T
OBSERVER
NIL
F
NIL
NIL
1

MONITOR
355
45
412
90
left
[ item 0 probs ] of subject
4
1
11

MONITOR
410
45
472
90
forward
[ item 1 probs ] of subject
4
1
11

MONITOR
470
45
527
90
right
[ item 2 probs ] of subject
4
1
11

BUTTON
645
45
705
78
sample
ask subject [ show sample-eff 1000 ]
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
1250
75
1450
225
sheep certainty
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
"default" 1.0 0 -16777216 true "" "plot-nz sheep-certainty"
"pen-1" 1.0 0 -7500403 true "" "plot-nz sheep-straight-certainty"
"pen-2" 1.0 0 -2674135 true "" "plot-nz sheep-left-certainty"
"pen-3" 1.0 0 -955883 true "" "plot-nz sheep-right-certainty"

PLOT
1250
250
1450
400
sheep probs
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
"default" 1.0 0 -7500403 true "" "plot-nz sheep-straight-prob"
"pen-1" 1.0 0 -2674135 true "" "plot-nz sheep-left-prob"
"pen-2" 1.0 0 -955883 true "" "plot-nz sheep-right-prob"

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

default-with-border
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250
Line -16777216 false 150 0 30 255
Line -16777216 false 45 255 150 210
Line -16777216 false 150 210 270 255
Line -16777216 false 150 0 270 255

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
NetLogo 6.3.0
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
  <experiment name="sheep-sweep-see-all" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fov" first="30" step="60" last="330"/>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolves-sweep-see-all" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fov" first="30" step="60" last="330"/>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="wolves-sweep-see-all-2-reps" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fov" first="30" step="60" last="330"/>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-mut-rate-see-all" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.001"/>
      <value value="0.003"/>
      <value value="0.01"/>
      <value value="0.03"/>
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="1"/>
      <value value="3"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-mut-rate-see-all-no-crossover-0.1-to-1.0" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mut-rate" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-mut-rate-see-all-crossover-0.1-to-1.0" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mut-rate" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="crossover?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sheep-see-all-mut-rate-0.3-100000-ticks" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>grass-eaten</metric>
    <metric>sheep-eaten</metric>
    <metric>sheep-born</metric>
    <metric>wolves-born</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>avg-grass</metric>
    <metric>avg-patches-with-sheep</metric>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bs-save-weights?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="include-null?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-06-10_sheep-mut-rate" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>sheep-left-prob</metric>
    <metric>sheep-straight-prob</metric>
    <metric>sheep-right-prob</metric>
    <metric>sheep-certainty</metric>
    <metric>sheep-left-certainty</metric>
    <metric>sheep-straight-certainty</metric>
    <metric>sheep-right-certainty</metric>
    <metric>wolf-left-prob</metric>
    <metric>wolf-straight-prob</metric>
    <metric>wolf-right-prob</metric>
    <metric>wolf-certainty</metric>
    <metric>wolf-left-certainty</metric>
    <metric>wolf-straight-certainty</metric>
    <metric>wolf-right-certainty</metric>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="210"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bs-save-weights?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="single-brain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-weights">
      <value value="&quot;random-normal 0 mut-rate&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-06-10_wolf-mut-rate" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>sheep-left-prob</metric>
    <metric>sheep-straight-prob</metric>
    <metric>sheep-right-prob</metric>
    <metric>sheep-certainty</metric>
    <metric>sheep-left-certainty</metric>
    <metric>sheep-straight-certainty</metric>
    <metric>sheep-right-certainty</metric>
    <metric>wolf-left-prob</metric>
    <metric>wolf-straight-prob</metric>
    <metric>wolf-right-prob</metric>
    <metric>wolf-certainty</metric>
    <metric>wolf-left-certainty</metric>
    <metric>wolf-straight-certainty</metric>
    <metric>wolf-right-certainty</metric>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="210"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bs-save-weights?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="single-brain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-weights">
      <value value="&quot;random-normal 0 mut-rate&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-06-24_sheep-mut-rate-500k" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>sheep-left-prob</metric>
    <metric>sheep-straight-prob</metric>
    <metric>sheep-right-prob</metric>
    <metric>sheep-certainty</metric>
    <metric>sheep-left-certainty</metric>
    <metric>sheep-straight-certainty</metric>
    <metric>sheep-right-certainty</metric>
    <metric>wolf-left-prob</metric>
    <metric>wolf-straight-prob</metric>
    <metric>wolf-right-prob</metric>
    <metric>wolf-certainty</metric>
    <metric>wolf-left-certainty</metric>
    <metric>wolf-straight-certainty</metric>
    <metric>wolf-right-certainty</metric>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="210"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bs-save-weights?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="single-brain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-weights">
      <value value="&quot;random-normal 0 mut-rate&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-06-24_wolf-mut-rate-500k" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500000"/>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>patches-with-sheep</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>sheep-left-prob</metric>
    <metric>sheep-straight-prob</metric>
    <metric>sheep-right-prob</metric>
    <metric>sheep-certainty</metric>
    <metric>sheep-left-certainty</metric>
    <metric>sheep-straight-certainty</metric>
    <metric>sheep-right-certainty</metric>
    <metric>wolf-left-prob</metric>
    <metric>wolf-straight-prob</metric>
    <metric>wolf-right-prob</metric>
    <metric>wolf-certainty</metric>
    <metric>wolf-left-certainty</metric>
    <metric>wolf-straight-certainty</metric>
    <metric>wolf-right-certainty</metric>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="210"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bs-save-weights?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="single-brain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="init-weights">
      <value value="&quot;random-normal 0 mut-rate&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-06-29_sheep-mut-rate-1M-by-100" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>repeat 100 [ go ]</go>
    <timeLimit steps="10000"/>
    <metric>ticks</metric>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>sheep-eat-attempts</metric>
    <metric>wolf-eat-attempts</metric>
    <metric>sheep-cert</metric>
    <metric>sheep-lcert</metric>
    <metric>sheep-scert</metric>
    <metric>sheep-rcert</metric>
    <metric>sheep-lnum</metric>
    <metric>sheep-snum</metric>
    <metric>sheep-rnum</metric>
    <metric>sheep-lprob</metric>
    <metric>sheep-sprob</metric>
    <metric>sheep-rprob</metric>
    <metric>wolf-cert</metric>
    <metric>wolf-lcert</metric>
    <metric>wolf-scert</metric>
    <metric>wolf-rcert</metric>
    <metric>wolf-lnum</metric>
    <metric>wolf-snum</metric>
    <metric>wolf-rnum</metric>
    <metric>wolf-lprob</metric>
    <metric>wolf-sprob</metric>
    <metric>wolf-rprob</metric>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="210"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bs-save-weights?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="single-brain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2023-06-29_sheep-mut-rate-1000-by-100" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go-n 100</go>
    <timeLimit steps="10"/>
    <metric>ticks</metric>
    <metric>count sheep</metric>
    <metric>count wolves</metric>
    <metric>grass</metric>
    <metric>sheep-efficiency</metric>
    <metric>sheep-escape-efficiency</metric>
    <metric>wolf-efficiency</metric>
    <metric>sheep-eat-attempts</metric>
    <metric>wolf-eat-attempts</metric>
    <metric>sheep-cert</metric>
    <metric>sheep-lcert</metric>
    <metric>sheep-scert</metric>
    <metric>sheep-rcert</metric>
    <metric>sheep-lnum</metric>
    <metric>sheep-snum</metric>
    <metric>sheep-rnum</metric>
    <metric>sheep-lprob</metric>
    <metric>sheep-sprob</metric>
    <metric>sheep-rprob</metric>
    <metric>wolf-cert</metric>
    <metric>wolf-lcert</metric>
    <metric>wolf-scert</metric>
    <metric>wolf-rcert</metric>
    <metric>wolf-lnum</metric>
    <metric>wolf-snum</metric>
    <metric>wolf-rnum</metric>
    <metric>wolf-lprob</metric>
    <metric>wolf-sprob</metric>
    <metric>wolf-rprob</metric>
    <enumeratedValueSet variable="mut-rate">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crossover?">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-random?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-random?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-grass?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-wolves?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolves-see-sheep?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fov">
      <value value="210"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="granularity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hidden-nodes">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="track-reactions?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vision">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-threshold">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bs-save-weights?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="single-brain?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-grass-density">
      <value value="0.35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="newborn-energy">
      <value value="0.1"/>
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
