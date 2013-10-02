(ns sudoku)

(def coords (range 0 81))

(defn row [board space]
  (keep-indexed
    #(if (= (quot %1 9) (quot space 9)) %2) board))

(defn column [board space]
  (keep-indexed
    #(if (= (mod %1 9) (mod space 9)) %2) board))

(defn square-index [space]
  (+ (quot (mod space 9) 3) (* 3 (quot space 27))))

(defn square [board space]
  (keep-indexed
    #(if (= (square-index %1) (square-index space)) %2) board))

(defn taken [board space]
  (set (concat (row board space) (column board space) (square board space))))

(defn not-taken [board space]
  (clojure.set/difference #{\1 \2 \3 \4 \5 \6 \7 \8 \9} (taken board space)))

(defn good-move? [board space]
  (= 1 (count (not-taken board space))))

(defn make-move [board space number]
  (clojure.string/join
    (assoc (vec (rest (clojure.string/split board #""))) space number)))

(defn unsolved-spaces [board]
  (keep
    #(if (= \0 (nth board %)) %) coords))

(defn assess-spot [board space]
  (cond (good-move? board space) (make-move board space (first (not-taken board space)))
  :else board))

(defn solved? [board]
  (empty? (unsolved-spaces board)))

(defn stuck? [board]
  (some #(empty? (not-taken board %)) (unsolved-spaces board)))

(declare solve)

(defn guess [board]
  (let [space (first (unsolved-spaces board))]
    (for [possibility (not-taken board space)]
      (let [new-board (make-move board space possibility)]
        (if-not (= false (solve new-board))
          (solve new-board))))))

(defn solve [board]
  (let [new-board (reduce assess-spot board (unsolved-spaces board))]
    (cond (solved? new-board) new-board
          (stuck? new-board) false
          (= new-board board) (flatten (filter identity (guess board)))
          :else (solve new-board))))


(println (solve "096040001100060004504810390007950043030080000405023018010630059059070830003590007"))
(println (solve "302609005500730000000000900000940000000000109000057060008500006000000003019082040"))
