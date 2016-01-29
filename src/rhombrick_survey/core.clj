(ns rhombrick-survey.core
  (:use [rhombrick.offline]
        [rhombrick.tilecode]
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


(defn tiling-uses-balanced-tile-distribution? [ts]
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


(defn -main [& args]
  (cond
    (zero? (count args))
      (println "error: usage <infile> and <outfile> [startidx]")
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
                        (println "")
                        ;(println "[QUALIFIED]")
                        (save-tiler-state new-state outfile))
                      (println "[REJECT]")
                      )))

              (swap! line-num inc)
            )
 
            (println "\nREACHED END OFFSET.. DONE!")
            )))))
      

