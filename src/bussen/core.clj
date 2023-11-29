(ns bussen.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.data]))

(defn check []
  (let [{ret :clojure.spec.test.check/ret sym :sym} (->> (stest/check) (filter :failure) first)]
    (if (:result ret)
      (do (println "Sym: " sym)
          (println "Problem:")
          (->> ret :result .getData :clojure.spec.alpha/problems first pprint))
      (println "Ok!"))))

(def suits #{:club :diamond :heart :spade})
(def ranks (into #{} (range 1 14)))
(def deck (for [suit suits
                rank ranks]
            {::rank rank
             ::suit suit}))

(defn distinct-coll? [coll]
  (if (empty? coll)
    true
    (apply distinct? coll)))

(defn distinct-cards-in-game [game]
  (->> (concat (::bussen game) (::draw-pile game))
       (map #(select-keys % [::rank ::suit]))
       distinct-coll?))

(s/def ::rank ranks)
(s/def ::suit suits)
(s/def ::card (s/keys :req [::rank ::suit]))
(s/def ::draw-pile (s/coll-of ::card :min-count 0 :distinct true))
(s/def ::visible boolean?)
(s/def ::play-card (s/merge ::card (s/keys :req [::visible])))
(s/def ::bussen (s/and (s/coll-of ::play-card :max-count 6)
                       #(= % (sort-by ::visible %))
                       (fn [cards] (->> cards (mapv #(select-keys % [::rank ::suit])) distinct?))))
(s/def ::game (s/and (s/keys :req [::bussen ::draw-pile])
                     distinct-cards-in-game))

(defn can-draw-card? [{::keys [draw-pile bussen] :as game}]
  (and (not (empty? draw-pile))
       (> 6 (count bussen))))

(s/fdef can-draw-card?
        :args (s/cat :game ::game)
        :ret boolean?)

(defn no-visible-cards? [game]
  (not-any? ::visible (::bussen game)))

(defn draw-card [{::keys [draw-pile] :as game}]
  (-> game
      (update-in [::bussen] conj (assoc (peek draw-pile) ::visible false))
      (update-in [::draw-pile] pop)))

(s/fdef draw-card
        :args (s/cat :game (s/and ::game
                                  can-draw-card?
                                  no-visible-cards?))
        :ret ::game
        :fn (fn [{{in :game} :args out :ret}]
              (let [[in-unique out-unique common] (clojure.data/diff in out)
                    only-top-some? #(and (some? (peek %))
                                         (every? nil? (pop %)))]
                (and (only-top-some? (in-unique ::draw-pile))
                     (only-top-some? (out-unique ::bussen))))))

(def hidden? (complement ::visible))

(defn discard-visible-cards [game]
  (update-in game [::bussen] #(take-while hidden? %)))

(s/fdef discard-visible-cards
        :args (s/cat :game ::game)
        :ret ::game
        :fn #(->> (:ret %) no-visible-cards?))

(defn replace-visible-cards [game]
  (let [after-discard (discard-visible-cards game)
        card-count-after-discard (count (::bussen after-discard))]
    (loop [g after-discard
           card-count card-count-after-discard]
      (if (and (can-draw-card? g) (< card-count 6))
        (recur (draw-card g) (inc card-count))
        g))))

(s/fdef replace-visible-cards
        :args (s/cat :game ::game)
        :ret ::game
        :fn (s/and #(->> (:ret %) no-visible-cards?)
                   (fn [{{in :game} :args
                         out :ret}]
                     (= (min
                         (+ (->> in ::bussen (filter hidden?) count)
                            (->> in ::draw-pile count))
                         6)
                        (->> out ::bussen count)))))

(defn game-won? [{bussen ::bussen draw-pile ::draw-pile}]
  (and (empty? draw-pile)
       (not (empty? bussen))
       (every? ::visible bussen)))

(s/fdef game-won?
        :args (s/cat :game ::game)
        :ret boolean?)

(defn prev-rank [bussen]
  (or (->> bussen
           (drop-while hidden?)
           first
           ::rank)
      7))

(s/fdef prev-rank
        :args (s/cat :bussen ::bussen)
        :ret int?)

(defn next-card [bussen]
  (->> bussen
       (take-while hidden?)
       last))

(s/fdef next-card
        :args (s/cat :bussen ::bussen)
        :ret (s/or :card ::card :nil nil?))

(defn reveal-next-card [bussen]
  (let [[hidden visible] (mapv #(into [] %) (split-with hidden? bussen))
        new-hidden (conj (pop hidden)
                         (assoc (peek hidden) ::visible true))]
    (concat new-hidden visible)))

(s/fdef reveal-next-card
        :args (s/cat :bussen (s/and ::bussen
                                    #(->> % (filter hidden?) count (< 0))))
        :ret ::bussen
        :fn (fn [{{in :bussen} :args out :ret}]
              (= (->> in (filter ::visible) count inc)
                 (->> out (filter ::visible) count))))

(defn format-card [{::keys [rank suit]}]
  (str (case rank
         1 \A 11 \J 12 \Q 13 \K
         rank)
       " "
       ({:spade "\u2660"
         :heart "\u2665"
         :diamond "\u2666"
         :club "\u2663"} suit)))

(defn new-game []
  (-> {::bussen []
       ::draw-pile (shuffle deck)}
      replace-visible-cards))

(s/fdef new-game
        :args empty?
        :ret ::game)

(defn read-guess []
  (case (read-line)
    "+" 1
    "-" -1
    "=" 0))

(defn format-bussen [bussen]
  (for [card bussen]
    (if (::visible card)
      (format-card card)
      "   ")))

(defn åk []
  (loop [game (new-game)]
    (let [bussen (::bussen game)]
      (cond
        (empty? bussen) (println "Game over!")
        (game-won? game) (println "You won!")
        :default (do (println (format-bussen bussen))
                     (let [guess (read-guess)
                           actual (next-card bussen)]
                       (println "Actual " (format-card actual))
                       (if (= guess (compare (::rank actual) (prev-rank bussen)))
                         (do (println "Correct!")
                             (recur (update-in game [::bussen] reveal-next-card)))
                         (do (println "Incorrect!")
                             (recur (-> game
                                        (update-in [::bussen] reveal-next-card)
                                        replace-visible-cards))))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
