(ns kachel.core)

(defn- boolean? [b]
  (= (boolean b) b))

(def square-grid-directions #{:up :down :left :right})

(defprotocol Grid
  (coordinate->field [this coordinate])
  (valid-coordinate? [this coordinate])
  (in-boundary? [this coordinate])
  (direction->coordinate-offset [this direction])
  (neighbours-coordinates [this position])
  (neighbours [this position])
  (neighbour-coordinate [this position direction])
  (neighbour [this position direction]))

(defn square-grid-coordinate->index [this [x y]]
  (assert (valid-coordinate? this [x y]))
  (let [x' (mod x (.width this))
        y' (mod y (.height this))]
    (+ x' (* y' (.width this)))))

(deftype SquareGrid [width height fields wrap-horizontal? wrap-vertical?]
  Grid
  (valid-coordinate? [_ [x y]]
    (and (or wrap-horizontal?
             (and (>= x 0) (< x width)))
         (or wrap-vertical?
             (and (>= y 0) (< y height)))))
  (in-boundary? [_ [x y]]
    (and (and (>= x 0) (< x width))
         (and (>= y 0) (< y height))))
  (coordinate->field [this coordinate]
    (get fields (square-grid-coordinate->index this coordinate)))
  (direction->coordinate-offset [_ direction]
    (case direction
      :up    [ 0 -1]
      :down  [ 0  1]
      :left  [-1  0]
      :right [ 1  0]))
  (neighbour-coordinate [this [x y] direction]
    (let [[x-offset y-offset] (direction->coordinate-offset this direction)
          x' (+ x x-offset)
          y' (+ y y-offset)]
      (when (valid-coordinate? this [x' y'])
        [(mod x' width)
         (mod y' height)])))
  (neighbour [this position direction]
    (coordinate->field this (neighbour-coordinate this
                                                  position
                                                  direction)))
  (neighbours-coordinates [this position]
    (vec (remove nil?
                 (mapv (partial neighbour-coordinate this position)
                       square-grid-directions))))
  (neighbours [this position]
    (mapv (partial coordinate->field this)
          (neighbours-coordinates this position))))


(defn square-grid [& {:keys [width
                             height
                             default
                             default-fn
                             wrap-vertical?
                             wrap-horizontal?]
                      :or {default nil
                           default-fn nil
                           wrap-vertical? false
                           wrap-horizontal? false}
                      :as args}]
  (assert (and (every? integer? [width height])
               (every? boolean? [wrap-vertical? wrap-horizontal?])
               (not (and (contains? args :default)
                         (contains? args :default-fn)))))
  (let [fields (if default-fn
                 (mapv default-fn
                       (for [y (range height)
                             x (range width)]
                         {:x x :y y :index (+ x (* y width))}))
                 (vec (take (* width height) (repeat default))))]
    (SquareGrid. width
                 height
                 fields
                 wrap-horizontal?
                 wrap-vertical?)))
