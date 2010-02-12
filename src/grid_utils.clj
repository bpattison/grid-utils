;   Copyright (c) Brian Pattison. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns grid-utils
  (:require [clojure.contrib.math :as math]))
  
; Bresenham's line algorithm  

; x major line
(defn- x-major-line [x x0 y0 adx ady x-dir y-dir]
  (if (= x0 x)
    [[x0 y0 0]]
    (let [r (x-major-line (+ x-dir x) x0 y0 adx ady x-dir y-dir)
          l (last r)]
      (let [[_ y e] l, e (+ e ady)]
        (if (>= (* e 2) adx)
          (conj r [x (+ y-dir y) (- e adx)])
          (conj r [x y e]))))))
          
; y major line
(defn- y-major-line [y x0 y0 adx ady x-dir y-dir]
  (if (= y0 y)
    [[x0 y0 0]]
    (let [r (y-major-line (+ y-dir y) x0 y0 adx ady x-dir y-dir)
          l (last r)]
      (let [[x _ e] l, e (+ e adx)]
        (if (>= (* e 2) ady )
          (conj r [(+ x-dir x) y (- e ady)])
          (conj r [x y e]))))))
          
(defn get-line [x0 y0 x1 y1]
  (let [dx  (- x1 x0)
        dy  (- y1 y0)
        m   (if (= dx 0)  ; check for divid by zero
              (* dy java.lang.Double/POSITIVE_INFINITY)
              (/ dy dx))
        adx (math/abs dx)
        ady (math/abs dy)
        am  (math/abs m)]
        
;   (println "[" x0 "," y0 "] [" x1 "," y1 "] m=" m)

    (cond
   
      ; q0 
      (and (< x0 x1) (<= 0 m 1))
        (x-major-line x1 x0 y0 adx ady -1 +1)
        
      ; q1          
      (and (< x0 x1) (and (> 0 m) (>= m -1)))
        (x-major-line x1 x0 y0 adx ady -1 -1)

      ; q4          
      (and (< x1 x0) (<= 0 m 1))
        (x-major-line x1 x0 y0 adx ady +1 -1)
        
      ; q5    
      (and (< x1 x0) (>= 0 m -1))
        (x-major-line x1 x0 y0 adx ady +1 +1)
          
      ; q2
      (and (< y1 y0) (> -1 m))
        (y-major-line y1 x0 y0 adx ady +1 +1)
      
      ; q3
      (and (< y1 y0) (< 1 m))
        (y-major-line y1 x0 y0 adx ady -1 +1)
      
      ; q6
      (and (< y0 y1) (> -1 m))  
        (y-major-line y1 x0 y0 adx ady -1 -1)
          
      ; q7
      (and (< y0 y1) (< 1 m))
        (y-major-line y1 x0 y0 adx ady +1 -1)
        
      ; no slope, not a line just a point
      ; happens when x0 == x1 and y0 == y1  
      true [])))
 
 (defn- test-get-line []
   (doseq [x (range -25 25 5) y (range -25 25 5)]
     (println (interpose "\n" (get-line 0 0 x y)))))

;(test-get-line)
