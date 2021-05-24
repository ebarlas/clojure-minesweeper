(ns minesweeper.core
  (:require [clojure2d.core :as c2d])
  (:gen-class))

(defn get-image
  [name]
  (c2d/load-image (str "assets/" name ".png")))

(def scales [[:small "small"] [:medium "medium"] [:large "large"]])

(def image-bgs (reduce into (map (fn [[scale name]] {scale (get-image (str "background_" name))}) scales)))
(def image-tile-cover (get-image "tile"))
(def image-tile-flag (get-image "tile_flag"))
(def image-tile-mine (get-image "tile_mine"))
(def image-tile-digits (vec (map get-image (map #(str "tile_" %) (range 9)))))
(def image-face-happy (get-image "face_playing"))
(def image-face-cool (get-image "face_win"))
(def image-face-sad (get-image "face_lose"))
(def image-digit-panel (get-image "digit_panel"))
(def image-digits (vec (map get-image (map #(str "digit_" %) (range 10)))))

(def layout
  {
   :grid-left          15
   :grid-top           81
   :cell-side          20
   :face-top           18
   :face-side          42
   :digit-panel-width  65
   :digit-panel-height 37
   :digit-panel-top    21
   :digit-panel-margin 2
   :digit-width        19})

(def layout-large
  (into layout {
                :scale            :large
                :width            630
                :height           416
                :face-left        273
                :flags-panel-left 20}))

(def layout-medium
  (into layout {
                :scale            :medium
                :width            350
                :height           416
                :face-left        154
                :flags-panel-left 20}))

(def layout-small
  (into layout {
                :scale            :small
                :width            210
                :height           276
                :face-left        84
                :flags-panel-left 16}))

(defn cell-keys
  ([rows cols]
   (cell-keys 0 rows 0 cols))
  ([row-from row-to col-from col-to]
   (for [r (range row-from row-to)
         c (range col-from col-to)]
     {:row r :col c})))

(defn empty-board
  [rows cols mines layout]
  (let [keys (cell-keys rows cols)
        cells (into {} (map (fn [key] {key {:mine false :adj-mines 0 :state :covered}}) keys))]
    {:left-clicked  false
     :right-clicked false
     :state         :playing
     :rows          rows
     :cols          cols
     :mines         mines
     :cells         cells
     :layout        layout}))

(defn from-layout
  [board key]
  (get-in board [:layout key]))

(defn place-mines
  ([board]
   (place-mines board (:mines board)))
  ([board mines]
   (if (= 0 mines)
     board
     (let [r (rand-int (:rows board))
           c (rand-int (:cols board))
           key {:row r :col c}
           cell (get-in board [:cells key])]
       (if (:mine cell)
         (place-mines board mines)
         (place-mines (assoc-in board [:cells key :mine] true) (- mines 1)))))))

(defn neighbor-keys
  [board row col]
  (let [nine-keys (cell-keys (- row 1) (+ row 2) (- col 1) (+ col 2))
        eight-keys (remove #(= % {:row row :col col}) nine-keys)]
    (filter #(get-in board [:cells %]) eight-keys)))

(defn neighbor-cells
  [board row col]
  (map #(get-in board [:cells %]) (neighbor-keys board row col)))

(defn count-covered
  [board]
  (reduce + (map #(if (contains? #{:covered :flagged} (:state %)) 1 0) (vals (:cells board)))))

(defn count-mines
  [board]
  (reduce + (map #(if (:mine %) 1 0) (vals (:cells board)))))

(defn count-flags
  [board]
  (reduce + (map #(if (= :flagged (:state %)) 1 0) (vals (:cells board)))))

(defn count-adj-mines
  [board row col]
  (reduce + (map #(if (:mine %) 1 0) (neighbor-cells board row col))))

(defn count-adj-flags
  [board row col]
  (reduce + (map #(if (= :flagged (:state %)) 1 0) (neighbor-cells board row col))))

(defn fill-adj-mines
  [board]
  (let [cells (into {} (map (fn [[k v]] {k (assoc v :adj-mines (count-adj-mines board (:row k) (:col k)))}) (:cells board)))]
    (assoc board :cells cells)))

(defn random-board
  [rows cols mines layout]
  (fill-adj-mines (place-mines (empty-board rows cols mines layout))))

(defn get-tile-image
  [cell]
  (cond
    (= (:state cell) :covered) image-tile-cover
    (= (:state cell) :flagged) image-tile-flag
    (:mine cell) image-tile-mine
    :else (get image-tile-digits (:adj-mines cell))))

(defn x-y-to-row-col
  [board x y]
  (let [col (int (Math/floor (/ (- x (get-in board [:layout :grid-left])) (get-in board [:layout :cell-side]))))
        row (int (Math/floor (/ (- y (get-in board [:layout :grid-top])) (get-in board [:layout :cell-side]))))]
    {:row row :col col}))

(defn x-y-to-row-col-cell
  [board x y]
  (let [row-col (x-y-to-row-col board x y)]
    [row-col (get-in board [:cells row-col])]))

(defn uncover-neighbors
  [board row col uncover-fn]
  (reduce
    (fn [board key]
      (let [cell (get-in board [:cells key])]
        (if (= :covered (:state cell))
          (uncover-fn board key)
          board)))
    board
    (neighbor-keys board row col)))

(defn set-start-time-if-unset
  [board]
  (if (not (:start-time board))
    (assoc board :start-time (System/currentTimeMillis))
    board))

(defn set-end-time
  [board]
  (assoc board :end-time (System/currentTimeMillis)))

(defn uncover-cell
  [board row-col]
  (let [board (set-start-time-if-unset (assoc-in board [:cells row-col :state] :uncovered))]
    (if (get-in board [:cells row-col :mine])
      (set-end-time (assoc board :state :lost))
      (if (= 0 (get-in board [:cells row-col :adj-mines]))
        (uncover-neighbors board (:row row-col) (:col row-col) uncover-cell)
        board))))

(defn clear-around-flags
  [board row col]
  (if (= (count-adj-mines board row col) (count-adj-flags board row col))
    (uncover-neighbors board row col uncover-cell)
    board))

(defn face-click?
  [board x y]
  (and
    (>= x (get-in board [:layout :face-left]))
    (< x (+ (get-in board [:layout :face-left]) (get-in board [:layout :face-side])))
    (>= y (get-in board [:layout :face-top]))
    (< y (+ (get-in board [:layout :face-top]) (get-in board [:layout :face-side])))))

(defn on-left-click
  [board x y]
  (let [[row-col cell] (x-y-to-row-col-cell board x y)]
    (cond
      (face-click? board x y) (random-board (:rows board) (:cols board) (:mines board) (:layout board))
      (not (= :playing (:state board))) board
      (= :uncovered (:state cell)) (clear-around-flags board (:row row-col) (:col row-col))
      (= :covered (:state cell)) (uncover-cell board row-col)
      :else board)))

(defn on-right-click
  [board x y]
  (let [[row-col cell] (x-y-to-row-col-cell board x y)]
    (if (and (= :playing (:state board)) (contains? #{:covered :flagged} (:state cell)))
      (assoc-in board [:cells row-col :state] (if (= :flagged (:state cell)) :covered :flagged))
      board)))

(defn left-click? [window] (and (= :left (c2d/mouse-button window)) (c2d/mouse-pressed? window)))
(defn right-click? [window] (and (= :right (c2d/mouse-button window)) (c2d/mouse-pressed? window)))

(defn handle-clicks
  [window board]
  (let [[x y] (c2d/mouse-pos window)]
    (if (and (>= x 0) (>= y 0))
      (if (left-click? window)
        (if (:left-clicked board)
          board
          (assoc (on-left-click board x y) :left-clicked true))
        (let [board (assoc board :left-clicked false)]
          (if (right-click? window)
            (if (:right-clicked board)
              board
              (assoc (on-right-click board x y) :right-clicked true))
            (assoc board :right-clicked false))))
      board)))

(defn check-winner
  [board]
  (if (and (= :playing (:state board)) (= (count-mines board) (count-covered board)))
    (set-end-time (assoc board :state :won))
    board))

(defn draw-digits
  [canvas digits right top width value]
  (when (> digits 0)
    (c2d/image canvas (get image-digits (rem value 10)) (- right width) top)
    (draw-digits canvas (- digits 1) (- right width) top width (int (/ value 10)))))

(defn draw-background
  [canvas board]
  (let [scale (from-layout board :scale)]
    (c2d/image canvas (scale image-bgs))))

(defn draw-flags-panel
  [canvas board]
  (c2d/image canvas image-digit-panel
             (from-layout board :flags-panel-left)
             (from-layout board :digit-panel-top))
  (draw-digits
    canvas
    3
    (+ (from-layout board :flags-panel-left) (from-layout board :digit-panel-width))
    (+ (from-layout board :digit-panel-margin) (from-layout board :digit-panel-top))
    (+ (from-layout board :digit-panel-margin) (from-layout board :digit-width))
    (- (count-mines board) (count-flags board))))

(defn get-elapsed-time
  [board]
  (if (not (:start-time board))
    0
    (if (:end-time board)
      (int (/ (- (:end-time board) (:start-time board)) 1000))
      (int (/ (- (System/currentTimeMillis) (:start-time board)) 1000)))))

(defn draw-time-panel
  [canvas board]
  (c2d/image canvas image-digit-panel
             (-
               (from-layout board :width)
               (from-layout board :flags-panel-left)
               (from-layout board :digit-panel-width))
             (from-layout board :digit-panel-top))
  (draw-digits
    canvas
    3
    (- (from-layout board :width) (from-layout board :flags-panel-left))
    (+ (from-layout board :digit-panel-margin) (from-layout board :digit-panel-top))
    (+ (from-layout board :digit-panel-margin) (from-layout board :digit-width))
    (get-elapsed-time board)))

(defn draw-face
  [canvas board]
  (let [left (from-layout board :face-left)
        top (from-layout board :face-top)]
    (if (= :playing (:state board))
      (c2d/image canvas image-face-happy left top)
      (if (= :lost (:state board))
        (c2d/image canvas image-face-sad left top)
        (c2d/image canvas image-face-cool left top)))))

(defn draw-tiles
  [canvas board]
  (doseq [col (range 0 (:cols board))
          row (range 0 (:rows board))]
    (let [cell (get-in board [:cells {:row row :col col}])]
      (c2d/image canvas
                 (get-tile-image cell)
                 (+ (from-layout board :grid-left) (* col (from-layout board :cell-side)))
                 (+ (from-layout board :grid-top) (* row (from-layout board :cell-side)))))))

(defn draw [canvas window frame board]
  (let [board (check-winner (handle-clicks window board))]
    (draw-background canvas board)
    (draw-flags-panel canvas board)
    (draw-time-panel canvas board)
    (draw-face canvas board)
    (draw-tiles canvas board)
    board))

(defn make-window
  [layout rows cols mines]
  (c2d/show-window {:canvas      (c2d/canvas (:width layout) (:height layout))
                    :window-name "Minesweeper"
                    :draw-state  (random-board rows cols mines layout)
                    :draw-fn     draw}))

(defn start
  [layout]
  (cond
    (= layout "small") (make-window layout-small 9 9 10)
    (= layout "medium") (make-window layout-medium 16 16 40)
    :else (make-window layout-large 16 30 99)))

(defn -main
  [& args]
  (start (or (first args) "small")))