;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Tragedy of Commons ver. 4.2 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  By  Young Joon Oh at UT Dallas

;;  11. 2 . 2010 


;; Focusing on "tick -> day" and "tick -> energy"


globals 
[dead new-sheep  ] 


breed [sheeps sheep ]
breed [sheep1s sheep1]
breed [sheep2s sheep2]
breed [sheep3s sheep3]


turtles-own [energy full ]
     
patches-own [green-rate grass-clock grass-clock1]



;;;;;;;;;;;;;;
;;; setup ;;;;
;;;;;;;;;;;;;;


to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  
 
 setup-patches
  


set-default-shape turtles "sheep"

create-sheeps number[
  set size 1.5
    set energy 35                                    
    set color white
  setxy random min-pxcor random max-pycor
  ]

ifelse sheep-y
[ create-sheep1s number [
    set size 1.5
  set energy 35
  set color yellow
  setxy random min-pxcor random min-pycor
  ] ][stop]

ifelse sheep-b
[   create-sheep2s number [
    set size 1.5
  set energy 35
  set color blue
  setxy random max-pxcor random max-pycor
   ]][stop]
 

ifelse sheep-r
[ create-sheep3s number [
    set size 1.5
  set energy 35
  set color red
  setxy random max-pxcor random min-pycor
   ]][stop]
 





end

  
    



to setup-patches
  ask patches [ if pxcor <= 5 and pxcor >= -5 and pycor <= 5 and pycor >= -5 [set pcolor green set green-rate 120 ]  
  
    
                                                                                                 ; Assume : 1 patch is 1 acre, 1 acre can feed 1 sheep.
  ]

end




;;;;;;;;;;;;
;;;;; go ;;;
;;;;;;;;;;;;



to go

 if ((sum [green-rate] of patches) / (121 * 120)) < 0.1 [type "No More Grass (Grass energy < 10 %)" stop ]
 if sum [energy] of turtles <= 0 [type "Sheep have No Energy" stop]
 
  move-sheeps
  eat-grass
  reproduce
  regrow-grass
  death
  regulations
  tick
  do-plots
  
end



to move-sheeps
  
  if not regulation [
  ask turtles [fd 1 face max-one-of patches [green-rate] set energy energy - 1] 
 ; ask turtles-on patches [fd 1 face one-of patches with [pcolor = green] set energy energy - 1]   ; problem which is solved
 
  ]
 


   end


to eat-grass
 
 ask turtles with [full = 0] [if green-rate > 0    [
          set energy (energy + 3 )                    ; 1 grass can feed 1 sheep for 1 year(360 ticks), so the amount of energy 1 grass can give a sheep is 120*3
      set green-rate (green-rate - 1) ]]
 
 ask turtles with [energy >= 360] [set full 1]
 ask turtles with [full >= 1] [ ifelse energy > 35 [set full full + 1  ] [set full 0] ] 
   
  ask patches with [pxcor <= 5 and pxcor >= -5 and pycor <= 5 and pycor >= -5] [ if green-rate <= 0 [set pcolor black set grass-clock1 0 set grass-clock grass-clock + 1]]
    
  ask patches with [green-rate > 0 and green-rate <= 30 and grass-clock = 0] [set pcolor green - 4 set grass-clock1 grass-clock1 + 1]
  ask patches with [green-rate > 30 and green-rate <= 90 and grass-clock = 0] [set pcolor green - 2 set grass-clock1 grass-clock1 + 1]
  ask patches with [green-rate > 90 and grass-clock = 0] [set pcolor green set grass-clock1 grass-clock1 + 1]
   
   
      
  end


  
  to regrow-grass
   
   ask patches with [grass-clock >= 1] [set grass-clock grass-clock + 1]
   
   ask patches with [grass-clock >= 1]  
   [if grass-clock >= 242 and grass-clock <= 362 and green-rate <= 120 [set grass-clock grass-clock + 1 set green-rate green-rate + 1]]
   
   ask patches [if grass-clock > 362 or green-rate > 120 [set grass-clock 0]]
   
   ask patches with [grass-clock1 >= 1 and grass-clock = 0] [if grass-clock1 >= 241 and green-rate < 120 [set green-rate green-rate + 1]]
   ask patches with [grass-clock1 >= 1 and grass-clock = 0] [if grass-clock1 >= 361 or green-rate >= 120 [set grass-clock1 0]]
     end  
  
    
    
    to reproduce
 
 if bring-new [ if not regulation [
   
   
    if sum [energy] of sheeps >= count sheeps * criterion and sum [green-rate] of patches > count sheeps * criterion [ create-sheeps 1 [set size 1.5 set color white setxy random min-pxcor random max-pycor set energy 35 set new-sheep new-sheep + 1]]
  
   if sum [energy] of sheep1s >= count sheep1s * criterion and sum [green-rate] of patches > count sheep1s * criterion and count sheep1s > 0 [ create-sheep1s 1 [set size 1.5 set color yellow setxy random min-pxcor random min-pycor set energy 35 set new-sheep new-sheep + 1]]
   
   if sum [energy] of sheep2s >= count sheep2s * criterion and sum [green-rate] of patches > count sheep2s * criterion and count sheep2s > 0 [ create-sheep2s 1 [set size 1.5 set color blue setxy random max-pxcor random max-pycor set energy 35 set new-sheep new-sheep + 1 ]]
   
   if sum [energy] of sheep3s >= count sheep3s * criterion and sum [green-rate] of patches > count sheep3s * criterion and count sheep3s > 0 [ create-sheep3s 1 [set size 1.5 set color red setxy random max-pxcor random min-pycor set energy 35 set new-sheep new-sheep + 1 ]]
   
 
   
]]
                
    end
  
  
    
    to regulations
      
      if regulation [
   
   ask sheeps [fd 1 face max-one-of patches with [pxcor >= -5 and pxcor <= 0 and pycor <= 5 and pycor >= 0] [green-rate]  set energy energy - 1]
   
   
   ask sheep1s [fd 1 face max-one-of patches with [pxcor >= -5 and pxcor <= 0 and pycor >= -5 and pycor <= 0] [green-rate]  set energy energy - 1]
   
  ask sheep2s [fd 1 face max-one-of patches with [pxcor >= 0 and pxcor <= 5 and pycor <= 5 and pycor >= 0] [green-rate]  set energy energy - 1]
  
  ask sheep3s [fd 1 face max-one-of patches with [pxcor >= 0 and pxcor <= 5 and pycor >= -5 and pycor <= 0] [green-rate]  set energy energy - 1]

  

 if bring-new [

 if sum [energy] of sheeps > count sheeps * criterion and sum [green-rate] of patches with [pxcor >= -5 and pxcor < 0 and pycor <= 5 and pycor > 0 ] > count sheeps * criterion
  [ create-sheeps 1 [set size 1.5 set color white setxy random min-pxcor random max-pycor set energy 35 set new-sheep new-sheep + 1]]
  
   if sum [energy] of sheep1s > count sheep1s * criterion and sum [green-rate] of patches with [pxcor >= -5 and pxcor < 0 and pycor >= -5 and pycor < 0 ] > count sheep1s * criterion
    [ create-sheep1s 1 [set size 1.5 set color yellow setxy random min-pxcor random min-pycor set energy 35 set new-sheep new-sheep + 1]]
   
   if sum [energy] of sheep2s > count sheep2s * criterion and sum [green-rate] of patches with [pxcor > 0 and pxcor <= 5 and pycor <= 5 and pycor > 0 ] > count sheep2s * criterion
    [ create-sheep2s 1 [set size 1.5 set color blue setxy random max-pxcor random max-pycor set energy 35 set new-sheep new-sheep + 1 ]]
   
   if sum [energy] of sheep3s > count sheep3s * criterion and sum [green-rate] of patches with [pxcor > 0 and pxcor <= 5 and pycor >= -5 and pycor < 0 ] > count sheep3s * criterion
    [ create-sheep3s 1 [set size 1.5 set color red setxy random max-pxcor random min-pycor set energy 35 set new-sheep new-sheep + 1 ]]
    
   
      ]]
      end 
    
to death
; ask turtles [if energy <= 0 [set dead dead + 1 die]]
end
  



;;;;;;;;;;;;;;;;;;;;
;;;;;; plot ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;



    
to do-plots
  set-current-plot-pen "sheep"
  plot count turtles
  
  
  set-current-plot-pen "grass"
  plot (count patches with [green-rate > 0]) 
  
  set-current-plot-pen "energy"
  plot sum [energy] of turtles / count turtles
  
  
  end

  
  
@#$#@#$#@
GRAPHICS-WINDOW
514
27
983
517
25
25
9.0
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
0
0
1
ticks
30.0

BUTTON
8
28
77
61
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
90
28
157
61
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
1

MONITOR
137
279
219
324
All sheep
count turtles\n
3
1
11

MONITOR
23
281
112
326
Grass
count patches with [green-rate > 0] 
0
1
11

SWITCH
1033
153
1143
186
sheep-y
sheep-y
0
1
-1000

SWITCH
1036
211
1146
244
sheep-b
sheep-b
0
1
-1000

SWITCH
1038
286
1148
319
sheep-r
sheep-r
0
1
-1000

PLOT
290
331
490
481
Energy
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
"grass" 1.0 0 -10899396 true "" ""
"energy" 1.0 0 -13345367 true "" ""
"sheep" 1.0 0 -2674135 true "" ""

MONITOR
18
392
218
441
Remaining Grass Energy(%)
((sum [green-rate] of patches) / (121 * 120)) * 100
1
1
12

MONITOR
21
336
138
385
Average Energy
sum [energy] of turtles / count turtles
0
1
12

MONITOR
248
277
369
326
# of new sheep
new-sheep
17
1
12

INPUTBOX
7
81
256
141
number
10
1
0
Number

SWITCH
1033
92
1154
125
bring-new
bring-new
0
1
-1000

SLIDER
38
212
210
245
criterion
criterion
35
360
140
5
1
NIL
HORIZONTAL

TEXTBOX
56
163
206
200
Sheeps' health level to bring new one
12
0.0
1

SWITCH
1040
384
1166
417
regulation
regulation
1
1
-1000

@#$#@#$#@
## TRAGEDY OF COMMONS MODEL By Young Joon Oh 

This model is made in Nov. 2 . 2010 by Young Joon Oh at UT Dallas



## INFORMATION 

Definition :

When actors >= 2 and they should share the resource, they abuse the resource because of actors' selfishness.  In the end, the resource gets exhausted.

Once the common has begun to be destroyed, it gets worse not only in the collective level, but in the individual level.   
Reducing the common causes to decrease the Grass (in this model). As a result, sheep's health also become worse.

To solve this problem, collective action is needed.


## EXPLANATION

Assumption :   
1 tick = 1 day, 1 year = 360 ticks, a sheep's survival needs 360 energy for 1 year.  
A sheep's initial energy = 35. 35 is basic energy for sheep.  
A sheep's gestation period = about 5 months. So, if a sheep has 150 energy, it is regarded the sheep is well health conditin. So, a criterion for ideal health condition  will be 150.


For simulation of Tragedy of Commons(ToC), it is very important to depict some factors as follows ;

(1) Re-growing grass

(2) Generating and Consuming Energy of Sheep and Grass

(3) Selfish Shepherds
	

For (1),  
Dr. Manske's paper indicates it takes about 4 months for full-grown grass on average.
I assume there is a common with full-grown grass.
So, in this model, Green Grass has 120 green-rate(energy) initially. If the green-rate is exhausted, it re-grows after 240 ticks(i.e. 120+240 = 360). 
Thus, even though the energy is not exhausted, the grass begins to re-grow after 240 ticks.


For (2),  
If a sheep gets to a grass, the sheep can get 3 units of energy, and the grass lose 1 unit of green-rate. However, the cumulative energy of sheep is 2, because it should consume 1 unit of energy to get to that grass.  

So, if a sheep's cumulative energy = 360, it means the sheep already has enough energy to live for 1 year. From 360 to the basic energy,35, the sheep do not need to get more energy from grass. This is called 'Full'.


For (3),  
Shepherds want to bring more sheep to the common, when some conditions are met.

First, 
all sheep's energy in the common  >  sheep's health criterion (like 150). 
It means all sheep he brought become very healthy. So, it is an incentive for a shepherd to bring more sheep for their health.

Second, 
energy of grass (green-rate)  > sheep's health criterion. 
It means there is sufficient grass in the common to feed extra sheep, without worsening health of the current sheep in the common.

When 'regulation' is operated, the whole common will be divided by 4 parts.   
Each kind of sheep can have own part of the common. After that, shepherds have just considered amount of grass in his own part of the common, not in the whole common.

If Grass energy < 10%, it stops ; It shows the tragedy of commons



## HOW TO USE

- Input - number : the initial number of each sheep

- Slider - criterion : the criterion of healthy sheep

- Switch - bring-new : if on, the shepherds can bring new sheep

- Switch - sheep-y, sheep-b, sheep-r : if on, yellow, blue, and red sheep will appear

- Switch - regulation : if on, each kind of sheep can be fed on its own grasses
	For white, north-west
	For yellow, south-west
	For blue, north-east
	For red, south-east.



## REFERENCES

Llewellyn L. Manske, Grass Growth in Height, http://www.chaps2000.com/bin/ch2r3.pdf

http://www.sheep101.info
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
Rectangle -1 true true 166 225 195 285
Rectangle -1 true true 62 225 90 285
Rectangle -1 true true 30 75 210 225
Circle -1 true true 135 75 150
Circle -7500403 true false 180 76 116

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
Rectangle -7500403 true true 195 106 285 150
Rectangle -7500403 true true 195 90 255 105
Polygon -7500403 true true 240 90 217 44 196 90
Polygon -16777216 true false 234 89 218 59 203 89
Rectangle -1 true false 240 93 252 105
Rectangle -16777216 true false 242 96 249 104
Rectangle -16777216 true false 241 125 285 139
Polygon -1 true false 285 125 277 138 269 125
Polygon -1 true false 269 140 262 125 256 140
Rectangle -7500403 true true 45 120 195 195
Rectangle -7500403 true true 45 114 185 120
Rectangle -7500403 true true 165 195 180 270
Rectangle -7500403 true true 60 195 75 270
Polygon -7500403 true true 45 105 15 30 15 75 45 150 60 120

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
setup
set grass? true
repeat 75 [ go ]
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
