

(def alphabet (seq " ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(def target-string "MORE GIDDY IN MY DESIRES THAN A MONKEY")

(defn rand-letter 
  "generate a random letter in the alphabet"
  [alphabet]
  (nth alphabet (rand-int (count alphabet))))

(defn random-phrase 
  "generate a random phrase in alphabet of length len
   phrase: a collection of letters. like (\\A \\B \\C)"
  [alphabet len]
  (take len (repeatedly #(rand-letter alphabet))))

(defn phrase->str
  "takes a phrase and returns a string"
  [phrase]
  (apply str phrase))

(defn str->phrase [str] (seq str))

(defn monkey 
  "monkeys will evolve until they get to the target-phrase or reach maximum-attempts
  what really distinguishes one monkey from another is the next-phrase-selector"
  [alphabet target-string max-attempts next-phrase-selector]
  (loop [target-phrase (str->phrase target-string)
         current-phrase (random-phrase alphabet (count target-phrase))
         attempt 0]
        (println (phrase->str current-phrase))
        (if (or (= attempt max-attempts) (= current-phrase target-phrase)) 
          (do
            (println)
            (println (phrase->str current-phrase))
            (println "attempts: " attempt)
            (phrase->str current-phrase))
          (recur target-phrase (next-phrase-selector alphabet target-phrase current-phrase) (inc attempt)))))

;; hoyle monkey

(defn hoyle-monkey [alphabet target-string]
  "hoyle-monkeys keep trying random phrases"
  (monkey
    alphabet
    target-string
    10000
    (fn [alphabet target-phrase _] 
        (random-phrase alphabet (count target-phrase)))))

;; darwin monkey

(defn mutate 
  "changes a random letter in the phrase with a single random letter"
  [alphabet phrase]
  (assoc (into [] phrase) (rand-int (count phrase)) (rand-letter alphabet)))

(defn next-generation
  "will take a phrase, and generate a collection of mutations of the phrase"
  [alphabet phrase size]
  (take size (repeatedly #(mutate alphabet phrase))))

(defn hamming-distance
  "finds the hamming distance of two phrases."
  [a b]
  (reduce + (map #(if (= %1 %2) 0 1) a b)))

(defn select
  "find the phrase in a generation that has the lowest hamming-distance to the target-phrase"
  [generation target-phrase]
  (let [scored (map #(vector (hamming-distance % target-phrase) %) generation) 
        min-pair (reduce (fn [p m] (if (< (first p) (first m)) p m)) scored)
        min-phrase (second min-pair)]
    min-phrase))
 
(defn darwin-monkey [alphabet target-string]
  "darwin monkeys produce a 'generation' of phrases that contain small random changes to a randomly generated string
  then select the phrase in the generation that is closest to the target. we repeat that until we arrive at the target string"
  (monkey
    alphabet
    target-string
    10000
    (fn [alphabet target-phrase current-phrase]
        (select (next-generation alphabet current-phrase 50) target-phrase))))

(defn- main [& args]
  (let [hoyle-result  (hoyle-monkey alphabet target-string)
        darwin-result (darwin-monkey alphabet target-string)]
    (println)
    (println "hoyle: " hoyle-result)
    (println "darwin:" darwin-result)))

