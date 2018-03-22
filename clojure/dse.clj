

(def alphabet (seq " ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def target-string "MORE GIDDY IN MY DESIRES THAN A MONKEY")
(def max-attempts 10000)

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
  "monkeys will evolve until they get to the target-phrase or reach maximum-attempts.
  what really distinguishes one monkey from another is the next-phrase-selector.
  next-phrase selector is a function that takes [alphabet target-phrase current-phrase] returns a new-phrase"
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

(defn hoyle-monkey 
  "hoyle-monkeys keep trying random phrases"
  [& {:keys [alphabet target-string max-attempts] 
      :or {alphabet alphabet, target-string target-string, max-attempts max-attempts}}]
  (monkey
    alphabet
    target-string
    max-attempts
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
  (->>
    generation
    (map #(vector (hamming-distance % target-phrase) %))  ;replace each mutation with (distance,mutation)
    (reduce (fn [m p] (if (< (first p) (first m)) p m)))  ;find the (distance,mutation) with smallest distance
    (second))) ;return the mutation
 
(defn darwin-monkey 
  "darwin monkeys produce a 'generation' of phrases that contain small random changes to a randomly generated string
  then select the phrase in the generation that is closest to the target. we repeat that until we arrive at the target string"
  [& {:keys [alphabet target-string max-attempts] 
      :or {alphabet alphabet, target-string target-string, max-attempts max-attempts}}]
  (monkey
    alphabet
    target-string
    max-attempts 
    (fn [alphabet target-phrase current-phrase]
        (select (next-generation alphabet current-phrase 50) target-phrase))))

(defn- main [& args]
  (let [hoyle-result  (hoyle-monkey)
        darwin-result (darwin-monkey)]
    (println)
    (println "hoyle: " hoyle-result)
    (println "darwin:" darwin-result)))

