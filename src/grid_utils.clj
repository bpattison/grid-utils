(ns grid-utils
  (:require [clojure.contrib.math :as math]))
  
; Bresenham's line algorithm  

; x major line
(defn x-major-line [x x0 y0 adx ady x-dir y-dir]
  (if (= x0 x)
    [[x0 y0 0]]
    (let [r (x-major-line (+ x-dir x) x0 y0 adx ady x-dir y-dir)
          l (last r)]
      (let [[_ y e] l, e (+ e ady)]
        (if (>= (* e 2) adx)
          (conj r [ x (+ y-dir y) (- e adx) ])
          (conj r [ x y e ]))))))
          
; y major line
(defn y-major-line [y x0 y0 adx ady x-dir y-dir]
  (if (= y0 y)
    [[x0 y0 0]]
    (let [r (y-major-line (+ y-dir y) x0 y0 adx ady x-dir y-dir)
          l (last r)]
      (let [[x _ e] l, e (+ e adx)]
        (if (>= (* e 2) ady )
          (conj r [ (+ x-dir x) y (- e ady) ])
          (conj r [ x y e ]))))))

          
(defn get-line [x0 y0 x1 y1]
  (let [dx  (- x1 x0)
        dy  (- y1 y0)
        m   (/ dy dx)
        adx (math/abs dx)
        ady (math/abs dy)
        am  (math/abs m)]
    (cond
      (and (< x0 x1) (<= 0 m 1))
        (do (println "q0 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (x-major-line x1 x0 y0 adx ady -1 +1))
          
      (and (< x0 x1) (and (> 0 m) (>= m -1)))
        (do (println "q1 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (x-major-line x1 x0 y0 adx ady -1 -1))
          
      (and (< x1 x0) (<= 0 m 1))
        (do (println "q4 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (x-major-line x1 x0 y0 adx ady +1 -1))
          
      (and (< x1 x0) (>= 0 m -1))
        (do (println "q5 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (x-major-line x1 x0 y0 adx ady +1 +1))
          
      (and (< y1 y0) (> -1 m))
        (do (println "q2 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (y-major-line y1 x0 y0 adx ady +1 +1))
          
      (and (< y1 y0) (< 1 m))
        (do (println "q3 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (y-major-line y1 x0 y0 adx ady -1 +1))
          
      (and (< y0 y1) (> -1 m))  
        (do (println "q6 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (y-major-line y1 x0 y0 adx ady -1 -1))
          
      (and (< y0 y1) (< 1 m))
        (do (println "q7 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (y-major-line y1 x0 y0 adx ady +1 -1))
      true
        (println "x0=" x0 "y0=" y0 "x1=" x1 "y1=" y1 "dx=" dx "dy=" dy "m=" m)
      )))


; x0 < x1
; 0 <= m <= 1
; quadrant #0
(defn- q0 [x x0 y0 dx dy]
  (if (= x0 x)                                    ; x0 x
    [[x0 y0 0]]
    (let [r (q0 (dec x) x0 y0 dx dy) l (last r)]  ; dec x
      (let [[_ y e] l, e (+ e dy)]                ; +dy
        (if (>= (* e 2) dx )                      ; >= dx
          (conj r [ x (inc y) (- e dx) ])         ; inc y, -dx
          (conj r [ x y e ]) )))))
          
; x0 < x1
; 0 > m >= -1
; quadrant #1
(defn- q1 [x x0 y0 dx dy]
  (if (= x0 x)                                    ; x0 x
    [[x0 y0 0]]
    (let [r (q1 (dec x) x0 y0 dx dy) l (last r)]  ; dec x
      (let [[_ y e] l, e (+ e dy)]                ; +dy
        (if (>= (* e 2) dx )                      ; >= dx
          (conj r [ x (dec y) (- e dx) ])         ; dec y, -dx
          (conj r [ x y e ]) )))))
          
; x1 < x0
; 0 <= m <= 1
; quadrant #4
(defn- q4 [x x0 y0 dx dy]
  (if (= x0 x)                                    ; x0 x
    [[x0 y0 0]]
    (let [r (q4 (inc x) x0 y0 dx dy) l (last r)]  ; inc x
      (let [[_ y e] l, e (+ e dy)]                ; +dy
        (if (>= (* e 2) dx)                       ; >= dx
          (conj r [ x (dec y) (- e dx) ])         ; dec y, -dx
          (conj r [ x y e ]) )))))
          
; x1 < x0
; 0 >= m >= -1
; quadrant #5
(defn- q5 [x x0 y0 dx dy]
  (if (= x0 x)                                    ; x0 x
    [[x0 y0 0]]
    (let [r (q5 (inc x) x0 y0 dx dy) l (last r)]  ; inc x
      (let [[_ y e] l, e (+ e dy)]                ; +dy
        (if (>= (* e 2) dx )                      ; >= dx
          (conj r [ x (inc y) (- e dx) ])         ; inc y, -dx
          (conj r [ x y e ]) )))))

;; -------- y - axis          
          
; y1 < y0
; -1 > m > -~ 
; quadrant #2
(defn- q2 [y x0 y0 dx dy]
  (if (= y0 y)                                    ; y0 y
    [[x0 y0 0]]
    (let [r (q2 (inc y) x0 y0 dx dy) l (last r)]  ; inc y
      (let [[x _ e] l, e (+ e dx)]                ; + dx
        (if (>= (* e 2) dy)                       ; >= dy
          (conj r [ (inc x) y (- e dy) ])         ; inc x, -dy
          (conj r [ x y e ]) )))))
          
; y1 < y0
; 1 < m < ~
; quadrant #3
(defn- q3 [y x0 y0 dx dy]
  (if (= y0 y)                                    ; y0 y
    [[x0 y0 0]]
    (let [r (q3 (inc y) x0 y0 dx dy) l (last r)]  ; inc y
      (let [[x _ e] l, e (+ e dx)]                ; + dx
        (if (>= (* e 2) dy)                       ; >= dy
          (conj r [ (dec x) y (- e dy) ])         ; dec x, -dy
          (conj r [ x y e ]) )))))
          
; y0 < y1
; -1 > m > -~ 
; quadrant #6
(defn- q6 [y x0 y0 dx dy]
  (if (= y0 y)                                    ; y0 y
    [[x0 y0 0]]
    (let [r (q6 (dec y) x0 y0 dx dy) l (last r)]  ; dec y
      (let [[x _ e] l, e (+ e dx)]                ; + dx
        (if (>= (* e 2) dy)                       ; >= dy
          (conj r [ (dec x) y (- e dy) ])         ; dec x, -dy
          (conj r [ x y e ]) )))))
          
; y0 < y1
; 1 <= m <= ~
; quadrant #7
(defn- q7 [y x0 y0 dx dy]
  (if (= y0 y)                                    ; y0 y
    [[x0 y0 0]]
    (let [r (q7 (dec y) x0 y0 dx dy) l (last r)]  ; dec y
      (let [[x _ e] l, e (+ e dx)]                ; + dx
        (if (>= (* e 2) dy )                      ; >= dy
          (conj r [ (inc x) y (- e dy) ])         ; inc x, -dy
          (conj r [ x y e ]) )))))

(defn m-line [x0 y0 x1 y1]
  (let [dx  (- x1 x0)
        dy  (- y1 y0)
        m   (/ dy dx)
        adx (math/abs dx)
        ady (math/abs dy)
        am  (math/abs m)]
    (cond
      (and (< x0 x1) (<= 0 m 1))
        (do (println "q0 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q0 x1 x0 y0 adx ady))
      (and (< x0 x1) (and (> 0 m) (>= m -1)))
        (do (println "q1 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q1 x1 x0 y0 adx ady))
      (and (< y1 y0) (> -1 m))
        (do (println "q2 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q2 y1 x0 y0 adx ady))
      (and (< y1 y0) (< 1 m))
        (do (println "q3 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q3 y1 x0 y0 adx ady))
      (and (< x1 x0) (<= 0 m 1))
        (do (println "q4 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q4 x1 x0 y0 adx ady))
      (and (< x1 x0) (>= 0 m -1))
        (do (println "q5 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q5 x1 x0 y0 adx ady))
      (and (< y0 y1) (> -1 m))  
        (do (println "q6 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q6 y1 x0 y0 adx ady))
      (and (< y0 y1) (< 1 m))
        (do (println "q7 [" x0 "," y0 "] [" x1 "," y1 "] m=" m)
          (q7 y1 x0 y0 adx ady))
      true
        (println "x0=" x0 "y0=" y0 "x1=" x1 "y1=" y1 "dx=" dx "dy=" dy "m=" m)
      )))

; q0
; x0 < x1
; 0 <= m <= 1
;(println (interpose "\n" (get-line 1 1 6 4)))

; q1
; x0 < x1
; 0 > m >= -1
;(println (interpose "\n" (get-line 0 0 6 -1)))

; q2
; y1 < y0
; -1 > m > -~
;(println (interpose "\n" (get-line 0 0 2 -7)))

; q3
; y1 < y0
; 1 > m > ~
;(println (interpose "\n" (m-line 0 0 -2 -7)))

; q4
; x1 < x0
; 0 <= m <= 1
;(println (interpose "\n" (m-line 6 4 1 1)))

; q5
; x1 < x0
; 0 >= m >= -1
;(println (interpose "\n" (m-line 0 0 -7 4)))

; q6
; y0 < y1
; -1 > m > ~
;(println (interpose "\n" (m-line 5 0 3 8)))

; q7
; y0 < y1
; 1 <= m <= ~
;(println (interpose "\n" (m-line 1 1 4 6)))
