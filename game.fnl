(var mytank-rot-d 2)
(var mytank-acc-d 300)
(var mybullet-v 500)
(var bullet-scale-reduct 10)
(var tank-line 150)
(var tank-friction 0.997)
(var tank-bounce 0.7)
(var window-width 1280)
(var window-height 1024)
(var power-max 5)
(var power-min 0.1)
(var item-spawn-interval 5)
(var enemy-spawn-interval 5)
(var items-max 5)
(var enemy-line 15)

(fn load-audio []
  {:boom (love.audio.newSource "boom.wav" "static")
   :hit (love.audio.newSource "hit.wav" "static")
   :pickup (love.audio.newSource "pickup.wav" "static")
   :shoot (love.audio.newSource "shoot.wav" "static")})

(fn init-mytank []
  {:x 600 :y 600 :scale 30 :rot 0 :vx 0 :vy 0 :power 0})

(fn init-state []
  {:mytank (init-mytank)
   :mybullets []
   :items []
   :enemies []
   :keys false
   :score 0
   :item-spawn-at item-spawn-interval
   :enemy-spawn-at enemy-spawn-interval
   :started false
   :audio (load-audio)})

(global max-score 0)
(global state (init-state))

(global font (love.graphics.newFont 90))

(fn new-bullet [t s v]
  {:x t.x
   :y t.y
   :scale s
   :rot 0
   :vx (* (math.cos t.rot) v)
   :vy (* (math.sin t.rot) v)})

(fn new-item []
  {:x (math.random 0 window-width)
   :y (math.random 0 window-height)
   :scale 10
   :rot 0
   :vx 0
   :vy 0})

(fn new-enemy []
  {:x (math.random 0 window-width)
   :y (math.random 0 window-height)
   :scale 10
   :rot 0
   :vx 0
   :vy 0})

(fn tank-field [t]
  (if (> t.power 0)
    (+ 30 (* t.power 60))
    0))

(fn is-gameover []
  (<= state.mytank.scale 0))

(fn set-score [s]
  (set state.score s)
  (when (< max-score s)
    (set max-score s)))

(fn pp [x] (print (fennel.view x)))

(fn love.load []
  (set state (init-state))
  (let [(success repl) (pcall require "lib.stdio")]
    (when success
      (repl.start))))

(fn rotate-tank [t d]
  (let
    [rot t.rot
     newrot (+ rot (* mytank-rot-d d))]
    (set t.rot newrot)))

(fn accel-tank [t d]
  (let
    [rot t.rot
     dvx (* mytank-acc-d d (math.cos rot))
     dvy (* mytank-acc-d d (math.sin rot))
     newvx (+ t.vx dvx)
     newvy (+ t.vy dvy)]
    (set t.vx newvx)
    (set t.vy newvy)))

(fn integrate-velocity [t d]
  (set t.x (+ t.x (* t.vx d)))
  (set t.y (+ t.y (* t.vy d))))

(fn apply-friction [t d]
  (set t.vx (* t.vx tank-friction))
  (set t.vy (* t.vy tank-friction)))

(fn wall-collision-tank [t]
  (let [wl t.scale
        wr (- window-width t.scale)
        wt t.scale
        wb (- window-height t.scale)]
    (when (< t.x wl)
      (set t.x wl)
      (set t.vx (math.abs (* t.vx tank-bounce -1)))
      (love.audio.play state.audio.hit))
    (when (> t.x wr)
      (set t.x wr)
      (set t.vx (- (math.abs (* t.vx tank-bounce -1))))
      (love.audio.play state.audio.hit))
    (when (< t.y wt)
      (set t.y wt)
      (set t.vy (math.abs (* t.vy tank-bounce -1)))
      (love.audio.play state.audio.hit))
    (when (> t.y wb)
      (set t.y wb)
      (set t.vy (- (math.abs (* t.vy tank-bounce -1))))
      (love.audio.play state.audio.hit))))

(fn inc-power [t dt]
  (set t.power (math.min power-max (+ t.power dt))))

(fn shoot [t bullets]
  (if (> t.power power-min)
      (do
        (table.insert bullets (new-bullet t (* t.power 20) (+ mybullet-v (* t.power 3))))
        (accel-tank t (* t.power -2))
        (love.audio.play state.audio.shoot)))
  (set t.power 0))

(fn reduce-scale [t d]
  (set t.scale (- t.scale d)))

(fn love.keypressed [key scancode isrepeat]
  (when (= key "s")
    (set state.keys true)))

(fn spawn-enemy []
  (table.insert state.enemies (new-enemy)))

(fn spawn-item []
  (table.insert state.items (new-item)))

(fn normalize-rot [a]
  (if (< a (* -1 math.pi))
      (+ a (* 2 math.pi))
      (> a (* 1 math.pi))
      (- a (* 2 math.pi))
      a))

(fn sign [a]
  (if (= a 0)
      0
      (< a 0)
      -1
      1))

(fn sub-rot [a b]
  (normalize-rot (- a b)))

(fn enemy-direction [t mt dt]
  (let [target-rot (math.atan2 (- mt.y t.y) (- mt.x t.x))
        diff-rot (sub-rot target-rot t.rot)
        sign-rot (sign diff-rot)]
    (set t.rot (+ t.rot (* sign-rot 2 dt)))))

(fn collide [t t2]
  (let [dx (- t.x t2.x)
        dy (- t.y t2.y)
        d (+ (* dx dx) (* dy dy))
        sscale (+ (* t.scale t.scale) (* t2.scale t2.scale))]
    (< d sscale)))

(fn infield [t t2]
  (let [field (tank-field t)
        dx (- t.x t2.x)
        dy (- t.y t2.y)
        d (+ (* dx dx) (* dy dy))
        sscale (+ (* field field) (* t2.scale t2.scale))]
    (< d sscale)))

(fn item-move [t t2 dt]
  (let [dx (- t2.x t.x)
        dy (- t2.y t.y)
        r (math.atan2 dy dx)
        tx (math.cos r)
        ty (math.sin r)]
    (set t.x (+ t.x (* tx dt 80)))
    (set t.y (+ t.y (* ty dt 80)))))

(fn damage [t t2]
  (let [s (- t.scale t2.scale)
        s2 (- t2.scale t.scale)]
    (set t.scale (math.max s 0))
    (set t2.scale (math.max s2 0))))

(fn love.update [dt]
  (let [isDown love.keyboard.isDown]
    (when (isDown "d")
      (rotate-tank state.mytank (* mytank-rot-d dt)))
    (when (isDown "a")
      (rotate-tank state.mytank (* mytank-rot-d dt -1)))
    (when (and (is-gameover) (isDown "a") (isDown "d") (not (isDown "s")))
      (set state (init-state)))
    (when (not (is-gameover))
      (if (isDown "s")
        (do
          (inc-power state.mytank dt)
          (when (not state.started)
             (set state.started true)))
        (shoot state.mytank state.mybullets)))
    (wall-collision-tank state.mytank)
    (apply-friction state.mytank dt)
    (integrate-velocity state.mytank dt)
    (when state.started
      (set state.enemy-spawn-at (- state.enemy-spawn-at dt))
      (when (< state.enemy-spawn-at 0)
        (spawn-enemy)
        (set state.enemy-spawn-at enemy-spawn-interval))
      (set state.item-spawn-at (- state.item-spawn-at dt))
      (when (and (< state.item-spawn-at 0) (< (length state.items) items-max))
        (spawn-item)
        (set state.item-spawn-at item-spawn-interval)))
    (each [_ bullet (ipairs state.mybullets)]
      (integrate-velocity bullet dt)
      (reduce-scale bullet (* dt bullet-scale-reduct)))
    (each [_ enemy (ipairs state.enemies)]
      (if (infield state.mytank enemy)
         (integrate-velocity enemy (* dt 0.2))
         (integrate-velocity enemy dt))
      (accel-tank enemy (* 0.1 dt))
      (enemy-direction enemy state.mytank dt)
      (wall-collision-tank enemy)
      (when (collide enemy state.mytank)
        (damage state.mytank enemy)
        (love.audio.play state.audio.boom))
      (each [_ bullet (ipairs state.mybullets)]
        (when (collide bullet enemy)
           (damage bullet enemy))))
    (each [_ item (ipairs state.items)]
      (when (infield state.mytank item)
        (item-move item state.mytank dt))
      (when (and (collide item state.mytank) (not (is-gameover)))
        (set-score (+ state.score item.scale (+ 1 (length state.enemies))))
        (set item.scale 0)
        (love.audio.play state.audio.pickup)))
    (set state.mybullets
      (icollect [_ bullet (ipairs state.mybullets)]
        (when (> bullet.scale 0)
         bullet)))
    (set state.enemies
      (icollect [_ enemy (ipairs state.enemies)]
        (when (> enemy.scale 0)
          enemy)))
    (set state.items
      (icollect [_ item (ipairs state.items)]
        (when (> item.scale 0)
         item)))
    (set state.keys false)))

(fn draw-tank [t]
  (when (> t.scale 0)
    (love.graphics.setColor 1 1 1)
    (love.graphics.circle "fill" t.x t.y t.scale) 
    (when (> t.power 0)
        (love.graphics.circle "line" t.x t.y (tank-field t)))) 
  (love.graphics.line
    t.x
    t.y
    (+ t.x (* (math.cos t.rot) tank-line))
    (+ t.y (* (math.sin t.rot) tank-line))))

(fn draw-item [t]
  (love.graphics.setColor 1 1 0)
  (love.graphics.circle "fill" t.x t.y t.scale)) 

(fn draw-bullet [t]
  (love.graphics.setColor 0.8 0.8 1)
  (love.graphics.circle "line" t.x t.y t.scale)) 

(fn draw-enemy [t]
  (love.graphics.setColor 1 0 0)
  (love.graphics.circle "fill" t.x t.y t.scale) 
  (love.graphics.line
    t.x
    t.y
    (+ t.x (* (math.cos t.rot) enemy-line))
    (+ t.y (* (math.sin t.rot) enemy-line))))

(fn draw-background []
  (love.graphics.clear 0.2 0.2 0.2))

(fn love.draw []
  (love.graphics.setFont font)
  (draw-background)
  (love.graphics.setColor 1 1 1)
  (love.graphics.print (.. "score:" state.score) 0 0 0 0.7 0.7)
  (love.graphics.print (.. "maxscore:" max-score) 0 70 0 0.7 0.7)
  (when (not state.started)
    (love.graphics.print "S is shot" (* window-width 0.3) (* window-height 0.3) 0 1 1)
    (love.graphics.print "A/D is rotate" (* window-width 0.3) (* window-height 0.6) 0 1 1)
    (love.graphics.print "RED is enemy" (* window-width 0.3) (* window-height 0.7) 0 1 1)
    (love.graphics.print "YELLOW is score" (* window-width 0.3) (* window-height 0.8) 0 1 1)
    (love.graphics.print "neguse 2025" (* window-width 0.3) (* window-height 0.95) 0 0.5 0.5))
  (when (is-gameover)
    (love.graphics.print "GAMEOVER" (* window-width 0.3) (* window-height 0.3) 0 1 1)
    (love.graphics.print "A+D to restart" (* window-width 0.3) (* window-height 0.6) 0 1 1))
  (draw-tank state.mytank)
  (each [_ bullet (ipairs state.mybullets)]
    (draw-bullet bullet))
  (each [_ item (ipairs state.items)]
    (draw-item item))
  (each [_ enemy (ipairs state.enemies)]
    (draw-enemy enemy)))

