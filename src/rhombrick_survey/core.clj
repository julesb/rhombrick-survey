(ns rhombrick-survey.core
  (:use [rhombrick.offline]
        [rhombrick.tilecode]
        [clojure.java.io])
  (:gen-class :main true)
  )

(def incount (atom 0))

(defn tiling-uses-all-tile-forms? [ts]
  (let [tilecodes (map #(normalize-tilecode (val %)) (ts :tiles))
        tileforms (into #{} tilecodes)
        tileset (set (get-in ts [:params :tileset]))
        ]
    (= tileforms tileset)
  ))


(defn -main [& args]
  (cond
    (zero? (count args))
      (println "error: need <infile> and <outfile>")
    (= (count args) 2)
      (do 
        (println "args:" args)

        (let [infile (first args)
              outfile (second args)]
          (println "in:" infile "out:" outfile)
          ;(println @state-idx "of" (count in-states) ":"
          ;     "tiles:" (count (tiling :tiles))
          ;     "iters:" (tiling :iters) "/" (params :max-iters)
          ;     "tileset:" (params :tileset)
          ;     "radius:" (get-assemblage-radius tiling)
          ;     "file:" filename)
          (reset! incount 0)
          (with-open [rdr (reader infile)]
            (doseq [line (line-seq rdr)]
              (swap! incount inc)
              ;(println @incount "in-state:" line)
              (let [initial-state (read-string line)]
                (println "[" @incount "]: input: " (initial-state :params))
                (let [new-state (make-tiling-best-of-n initial-state 2)]
                  ;(println "new state: " (pr-str new-state))
                  (when (and
                          (true? (new-state :solved))
                          (> (count (new-state :tiles)) 1)
                          (tiling-uses-all-tile-forms? new-state)
                          )
                    (save-tiler-state new-state outfile)))
              ))))


          ;(println (make-tiling (make-tiler-state (make-params :tileset ["-----1---1-1"]))))

          ;;(doseq [code (generate-normalized-tilecode-set2 start end)]
          ;(doseq [code (generate-normalized-tilecode-permutations start )]
          ;  (println code)
          ;  ;(swap! iter inc)
          ;  ;(when (zero? (mod @iter 1000))
          ;  ;  (println "#### Checkpoint: " code))
          ;)
          ;(println "done start:" start "end:" end)
          )))
      

