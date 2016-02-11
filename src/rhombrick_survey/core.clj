(ns rhombrick-survey.core
  (:use [rhombrick.offline]
        [rhombrick.tilecode]
        [rhombrick.tiling]
        [rhombrick.staticgeometry]
        [clojure.java.io]
        [clojure.math.combinatorics])
  (:gen-class :main true)
  )

(def line-num (atom 0))
(def start-offset (atom 1))
(def end-offset (atom 0x1000000000000))
(def num-to-process (atom 0))

(defn tiling-uses-all-tile-forms? [ts]
  (let [tilecodes (map #(normalize-tilecode (val %)) (ts :tiles))
        tileforms (into #{} tilecodes)
        tileset (set (get-in ts [:params :tileset]))
        ]
    (= tileforms tileset)))


(defn tiling-balanced? [ts]
  (let [min-ratio 0.1
        tiles-norm (->> (vals (ts :tiles)) ; (map #(second (val %) (ts :tiles))
                        (map normalize-tilecode)
                        )
        freqs (frequencies tiles-norm)
        tiles-forms (keys freqs)
        sel-forms (selections tiles-forms 2)
        ratios (map #(/ (+ (freqs (first %)) 0.00000001)
                        (+ (freqs (second %)) 0.00000001))
                    sel-forms)
        answer (and (= (count tiles-forms)
                       (count (get-in ts [:params :tileset])))
                    (zero? (count (filter #(< % min-ratio) ratios))))]
    answer))


(defn validate-state [ts]
  (let [tileset (get-in ts [:params :tileset])]
    (and
      ;true
      (= (count tileset) 2)
      (> (count (ts :tiles)) 2)
      ;(= (ts :solved) true)
      (tiling-balanced? ts)
      (not (zero? (compare (first tileset) "------------")))
      (not (zero? (compare (second tileset) "------------")))
      
      (or ; reject tilesets that dont have at least one tile with at least 3 conns 
        (> (get-num-connected (first tileset)) 2)
        (> (get-num-connected (second tileset)) 2))
      )
  ))

(defn tiler-can-iterate?2 [ts]
  (and (= (ts :run-status) :runnable)
       (< (count (ts :tiles)) (get-in ts [:params :max-tiles]))
       (< (ts :iters) (get-in ts [:params :max-iters]))
       (not (and (= (ts :iters) 256) (< (count (ts :tiles)) 13))) ; early bailout
       ))

(defn make-tiling2 [ts]
  (if (tiler-can-iterate?2 ts)
    (recur (make-backtracking-tiling-iteration4 ts))
    ts))


(defn make-tiling-best-of-n2 [ts n]
  (let [tilings (->> (pmap (fn [_] (make-tiling2 ts)) (range n))
                     (filter validate-state)
                     (sort-by #(count (% :tiles)))
                     (last))
        ]

    ;(println "generated" n "tilings")
    tilings
    ))


(defn generate-random-tiling [idx]
  (set-topology :rhombic-dodecahedron)
  (let [tileset (make-random-compatible-tileset 2)
        initial-state (->> (make-params :tileset tileset
                                        :seed (rand-nth tileset)
                                        :max-iters 4096
                                        :max-radius 8
                                        :max-tiles 200)
                           (make-state))
        ]
    (print (str (format "[%d] " idx)
           (get-in initial-state [:params :tileset]) " "))
    (make-tiling-best-of-n2 initial-state 2)
    )
  )

(defn run-random [n outfile]
  (doseq [i (range n)]
    (if-let [tiling (generate-random-tiling i)]
      (do
        (if (tiling :solved)
          (print (str (format "[SOLVED] [i:%d t:%d] "
                              (tiling :iters) (count (tiling :tiles)))))
          (print (str (format "[UNSOLVED] [i:%d t:%d] "
                              (tiling :iters) (count (tiling :tiles))))))
        (when (tiling :solved)
              ;(or (tiling :solved)
              ;    (> (count (tiling :tiles)) 13))

          (print "[SAVE]")
          (save-tiler-state tiling outfile)
        )
        (println))
      (println "[REJECT]"))
  ))


(defn -main [& args]
  (cond
    (zero? (count args))
      (do
        (run-random 64 "rd-twotiles-randomsurvey.tiling")
        (shutdown-agents))
      ;(println "error: usage <infile> and <outfile> [startidx]")
    (>= (count args) 2)
      (do 
        (println "args:" args)

        (let [infile (first args)
              outfile (second args)]
          (println "in:" infile "out:" outfile)
          
          (when (>= (count args) 3)
            (reset! start-offset (Integer/parseInt (nth args 2)))
            (println "ARG: start-offset:" @start-offset)
            )

          (when (>= (count args) 4)
            (reset! end-offset (Integer/parseInt (nth args 3)))
            (println "ARG: end-offset:" @end-offset)
            )
          
          (reset! line-num @start-offset)
          (reset! num-to-process (- @end-offset @start-offset))
          
          (with-open [rdr (reader infile)]
            (doseq [line (take @num-to-process
                               (drop (dec @start-offset)
                                     (line-seq rdr)))]

              ;(if (< @line-num @end-offset)
                (let [initial-state (read-string line)]
                  (print (str (format "[%d] " @line-num)
                              (get-in initial-state [:params :tileset]) " "))

                  (let [new-state (-> (make-tiling-best-of-n initial-state 2)
                                      (assoc :input-seq-id @line-num))
                        solved? (new-state :solved)]
                    (if solved?
                      (print (str (format "[SOLVED] [i:%d t:%d] "
                                          (new-state :iters)
                                          (count (new-state :tiles)))))
                      (print (str (format "[UNSOLVED] [i:%d t:%d] "
                                          (new-state :iters)
                                          (count (new-state :tiles))))))
                    (if (and
                          true
                          ;solved?
                          ;(tiling-uses-balanced-tile-distribution? new-state)
                          )
                      (do
                        ;(println "[QUALIFIED]")
                        (save-tiler-state new-state outfile)
                        (println "")
                        )
                      (println "[REJECT]")
                      )))

              (swap! line-num inc)
            )
            )
          (println "\nREACHED END OFFSET.. DONE!")
          (shutdown-agents)
          
          ))))
      

