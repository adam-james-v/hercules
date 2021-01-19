(ns ^:figwheel-hooks hercules.main
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [reagent.ratom :refer [reaction]]
            [svg-clj.main :as svg]
            [same :refer [ish? zeroish?]]))

(def v+ (partial mapv +))
(def v- (partial mapv -))
(def v* (partial mapv *))

(defn distance
  "compute distance between two points"
  [a b]
  (let [v (v- b a)
        v2 (reduce + (v* v v))]
    (Math/sqrt v2)))

(defn normalize
  "find the unit vector of a given vector"
  [v]
  (let [m (Math/sqrt (reduce + (v* v v)))]
    (mapv / v (repeat m))))

(defn perpendicular-2d
  [[x y]]
  [(- y) x])

(defn determinant-2d
  [a b]
  (- (* (first a) (second b))
     (* (second a) (first b))))

(defn line-intersection
  [[a b] [c d]]
  (let [[ax ay] a
        [bx by] b
        [cx cy] c
        [dx dy] d
        xdiff [(- ax bx) (- cx dx)]
        ydiff [(- ay by) (- cy dy)]
        div (determinant-2d xdiff ydiff)]
    (when (not (zeroish? (Math/abs div))) 
      (let [d [(determinant-2d a b) (determinant-2d c d)]
            x (/ (determinant-2d d xdiff) div)
            y (/ (determinant-2d d ydiff) div)]
        [x y]))))

(defn offset-edge
  [[a b] d]
  (let [p (perpendicular-2d (v- b a))
        pd (v* (normalize p) (repeat (- d)))
        xa (v+ a pd)
        xb (v+ b pd)]
    [xa xb]))

(defn cycle-pairs
  [pts]
  (let [n (count pts)]
    (vec (take n (partition 2 1 (cycle pts))))))

(defn every-other
  [v]
  (let [n (count v)]
    (map #(get v %) (filter even? (range n)))))

(defn wrap-list-once
  [s]
  (conj (drop-last s) (last s)))

(defn offset
  [pts d]
  (let [edges (cycle-pairs pts)
        opts (mapcat #(offset-edge % d) edges)
        oedges (every-other (cycle-pairs opts))
        edge-pairs (cycle-pairs oedges)]
    (wrap-list-once (map #(apply line-intersection %) edge-pairs))))

(def patterns-component
  [:svg 
   {:style {:width 0 :height 0 :margin 0 :padding 0 :display "block"}}
   [:defs
    [:linearGradient#gradient {:x1 0 :y1 0 :x2 1 :y2 -0.1}
     [:stop {:offset "0%" :stop-color "rgba(0,0,0,0.6)"}]
     [:stop {:offset "20%" :stop-color "rgba(180,155,70,0.2)"}]
     [:stop {:offset "37%" :stop-color "rgba(0,0,0,0.3)"}]
     [:stop {:offset "85%" :stop-color "rgba(180,155,70,0.4)"}]]]])

(def boon-frame
  (let [extents [ [0 0] [200 0] [200 200] [0 200] ]
        corner-dim 14
        opts (concat 
              [ [0 corner-dim] [corner-dim corner-dim] [corner-dim 0] ]
              (rest extents))
        tri [ [0 0] [28 0] [28 28] ]
        rct [ [0 0] [35 0] [35 22] [0 22] ]]
     (->> (svg/g
           (->> (svg/polygon-path extents)
                (svg/style {:fill "none" :stroke "none"}))
           (->> (svg/merge-paths
                 (svg/polygon-path opts)
                 (svg/polygon-path (offset extents -16)))
                (svg/style 
                 {:style {:filter "drop-shadow(0px 0px 8px rgba(225,200,110,0.9))"}}))
           (->> (svg/merge-paths
                 (svg/polygon-path opts)
                 (svg/polygon-path (offset extents -20)))
                (svg/style {:fill "rgb(120,110,90)" 
                                    :stroke "goldenrod" :stroke-width "3px"}))
           (->> (svg/merge-paths
                 (svg/polygon-path (offset opts -4))
                 (svg/polygon-path (offset extents -20)))
                (svg/style {:fill "rgb(50,30,10)" :stroke "none"}))
           (->> (svg/merge-paths
                 (svg/polygon-path (offset opts -12))
                 (svg/polygon-path (offset extents -20)))
                (svg/style {:fill "rgb(180,155,70)" :stroke "none"}))
           
           (->> (svg/g
                 (->> (svg/merge-paths
                       (svg/polygon-path tri)
                       (svg/polygon-path (offset tri -6)))
                      (svg/translate [-9 181])
                      (svg/rotate 180.1))
                 (->> (svg/merge-paths
                       (svg/polygon-path tri)
                       (svg/polygon-path (offset tri -5)))
                      (svg/translate [172 0]))
                 (->> (svg/merge-paths
                       (svg/polygon-path rct)
                       (svg/polygon-path (offset rct -8)))
                      (svg/translate [50 0]))
                 (->> (svg/merge-paths
                       (svg/polygon-path rct)
                       (svg/polygon-path (offset rct -6)))
                      (svg/translate [115 178]))
                 (->> (svg/merge-paths
                       (svg/polygon-path rct)
                       (svg/polygon-path (offset rct -8)))
                      (svg/translate [172 130])
                      (svg/rotate 90.1))
                 (->> (svg/merge-paths
                       (svg/polygon-path rct)
                       (svg/polygon-path (offset rct -4)))
                      (svg/translate [-6 135])
                      (svg/rotate 90.1))
                 (->> (svg/merge-paths
                       (svg/polygon-path rct)
                       (svg/polygon-path (offset rct -4)))
                      (svg/translate [-6 35])
                      (svg/rotate 90.1))
                 (->> (svg/merge-paths
                       (svg/polygon-path rct)
                       (svg/polygon-path (offset rct -4)))
                      (svg/translate [150 0])))
                (svg/style {:fill "rgb(100,95,70)" :stroke "none"}))

           (->> (svg/merge-paths
                 (svg/polygon-path (offset extents -20))
                 (svg/polygon-path (offset extents -26)))
                (svg/style {:fill "rgb(0,0,0)" :stroke "none"}))
           (->> (svg/polygon-path opts)
                (svg/style {:fill "url(#gradient)" :stroke "none"}))
           (->> (svg/merge-paths
                 (svg/polygon-path opts)
                 (svg/polygon-path (offset extents -23)))
                (svg/style {:fill "rgba(249,235,105,0.2)" :stroke "none"})))
          (svg/translate [-100 -100])
          (svg/rotate 44.999))))

(defn label
  [font-size text]
  [:text 
   {:x 0 :y 0 
    :style {:font-family "Verdana", 
            :font-size font-size}} text])

(defn boon-details
  [name creps rreps ereps hreps]
  (svg/g
   (->> (svg/polyline [ [-160 160] [0 0] [1000 0] ])
        (svg/style {:fill "none"
                            :stroke "goldenrod"
                            :stroke-width "2px"
                            :stroke-linecap "round"}))
        (->> (svg/g
              (->> (label 44 name) (svg/translate [140 -50]))
              (->> (label 28 (str "Common: " creps)) (svg/translate [145 -12]))
              (->> (label 28 (str "Rare: " rreps)) (svg/translate [365 -12]))
              (->> (label 28 (str "Epic: " ereps)) (svg/translate [525 -12]))
              (->> (label 28 (str "Heroic: " hreps)) (svg/translate [675 -12])))
             (svg/style {:fill "rgb(240,240,240)"}))))

(defn boon-art
  [bg-col name element]
  (svg/g
   (->> (svg/g 
         (->> (svg/rect 175 175)
              (svg/rotate 45)
              (svg/style {:fill bg-col}))
         element
         boon-frame)
        (svg/style {:class "boon"}))
   (->> (label 26 name)
        (svg/translate [-120 10])
        (svg/rotate -45)
        (svg/style {:class "info"}))))

;; {:name "name" :reps [1 2 3 4] :icon 'symbol :color "rgb(20,20,20)"}
;; boon-option is 1 of 3 choices available during any selection (the map containing the data has already been rolled when passed into the render
(defn boon-option
  [{:keys [name reps icon color]}]
  (let [[cr rr er hr] reps]
    (->> (svg/g
          (boon-art color name icon)
          (->> (boon-details name cr rr er hr) 
               (svg/translate [175 -10]))))))

(defn boon-column
  [exs]
  (let [exs (vec exs)]
    (into
     [:g {}]
     (for [idx (range (count exs))]
       (let [{:keys [name icon color]} (get exs idx)]
         (->> (boon-art color name icon)
              (svg/translate [0 (* idx 320)])))))))

(defn boon-overview
  [exs]
  (svg/svg
   [700 700 0.6]
   (svg/translate 
    [160 160] 
    (into [:g {}]
          (for [[col idx] 
                (partition 2 (interleave (partition-all 3 exs) (range)))]
            (->> (boon-column col)
                 (svg/translate [(* idx 160) (if (odd? idx) 160 0)])))))))

(defn boon-choices
  [a b c]
  (svg/svg
   [700 550 0.5]
   (when (and (not (nil? a))
              (not (nil? b))
              (not (nil? c)))
       (->> (svg/g
             (->> (boon-option a) (svg/translate [0 0]))
             (->> (boon-option b) (svg/translate [0 353]))
             (->> (boon-option c) (svg/translate [0 706])))
            (svg/translate [200 200])))))

(def equipment
  #{:rings :pull-up-bar :dumbbells :barbell :jump-rope :bands})

(def core-col "rgb(170,145,80)") ;; athena
(def arms-col "rgb(146,128,210)") ;; dionysus
(def legs-col "rgb(40,90,140)") ;; poseidon
(def back-col "rgb(170,134,30)") ;; zeus
(def chest-col "rgb(140,60,60)") ;; ares
(def speed-col "rgb(255,137,137)") ;; hermes
(def yoga-col "rgb(221,110,200)") ;; aphrodite

(def fist-icon (svg/image "fist-icon.png" 250 250))
(def pullup-icon (svg/image "pullup-icon.png" 250 250))
(def chinup-icon (svg/image "chinup-icon.png" 250 250))
(def curl-icon (svg/image "curl-icon.png" 250 250))
(def pistol-squat-icon (svg/image "pistol-squat-icon.png" 250 250))
(def diamond-pushup-icon (svg/image "diamond-pushup-icon.png" 250 250))
(def chest-flye-icon (svg/image "chest-flye-icon.png" 250 250))
(def squat-jump-icon (svg/image "squat-jump-icon.png" 250 250))
(def high-knee-icon (svg/image "high-knee-icon.png" 250 250))
(def wide-pushup-icon (svg/image "wide-pushup-icon.png" 250 250))
(def pushup-icon (svg/image "pushup-icon.png" 250 250))
(def boat-crunch-icon(svg/image "boat-crunch-icon.png" 250 250))
(def leg-raise-icon (svg/image "leg-raise-icon.png" 250 250))
(def mountain-climber-icon (svg/image "mountain-climber-icon.png" 250 250))
(def dip-icon (svg/image "dip-icon.png" 250 250))

(def exercises
  [{:name "Pullups"
    :ranges [2 4 7 10]
    :equipment [:rings :pbar]
    :color back-col :icon pullup-icon}

   {:name "Chinups"
    :ranges [2 4 7 10]
    :color arms-col :icon chinup-icon}

   {:name "Curls"
    :ranges [5 7 12 16]
    :color arms-col :icon curl-icon}

   {:name "Chest Flyes"
    :ranges [5 7 12 16]
    :color chest-col :icon chest-flye-icon}

   {:name "Squat Jumps"
    :ranges [5 10 15 20]
    :color speed-col :icon squat-jump-icon}

   {:name "Squat High Knee"
    :ranges [5 10 15 20]
    :color legs-col :icon high-knee-icon}

   {:name "Diamond Pushups"
    :ranges [3 7 10 15]
    :color arms-col :icon diamond-pushup-icon}

   {:name "Wide Stance Pushups"
    :ranges [3 7 10 15]
    :color back-col :icon wide-pushup-icon}

   {:name "Pushups"
    :ranges [4 8 12 20]
    :color chest-col :icon pushup-icon}

   {:name "Boat Crunches"
    :ranges [8 12 25 35]
    :color core-col :icon boat-crunch-icon}

   {:name "Leg Raises"
    :ranges [2 4 7 10]
    :color core-col :icon leg-raise-icon}

   {:name "Mountain Climbers"
    :ranges [10 14 18 22]
    :color speed-col :icon mountain-climber-icon}

   {:name "Dips"
    :ranges [3 7 10 15]
    :color chest-col :icon dip-icon}

   {:name "Pistol Squats"
    :ranges [3 7 10 15]
    :color legs-col :icon pistol-squat-icon}])

(defn rand-value
  [v]
  (get v (rand-int (count v))))

(defn roll-ranges
  [exercise]
  (let [{:keys [name ranges]} exercise
        [c r e] (mapv #(into [] (apply range %))
                      (partition 2 1 ranges))]
    {:name name
     :common (rand-value c)
     :rare (rand-value r)
     :epic (rand-value e)
     :heroic (apply max ranges)}))

(defn boon
  [exercise]
  (let [{:keys [name common rare epic heroic]} (roll-ranges exercise)]
    (-> exercise
        (assoc :reps [common rare epic heroic])
        (dissoc :ranges))))

(def state (r/atom [(boon (rand-value exercises))
                    (boon (rand-value exercises))
                    (boon (rand-value exercises))]))

(defn re-roll
  []
  (reset! state [(boon (rand-value exercises))
                 (boon (rand-value exercises))
                 (boon (rand-value exercises))]))

(def controls
  [:<>
   [:div
    [:button {:style {:margin-right "20px"} :onClick re-roll} "ROLL"]
    [:button {:onClick #(reset! state nil)} "HIDE"]]])

(def blurb
  [:<>
   [:h2 {:style {:color "goldenrod"}} "Exercises"]
   [:div (boon-overview exercises)]
   [:h1 "Hercules"]
   [:h5 "Use the game " [:em "Hades"] " to become strong in real life."]
   [:ol
    [:li "Start a Hades run."]
    [:li "At every boon choice, roll exercise options."]
    [:li "Perform the exercise corresponding to your boon choice in game."]
    [:li "Use the boon rarity to determine the number of reps to complete."]
    [:li "Have fun and modify exercises to your skill level!"]]
   [:p "There is no shame in any level of fitness. The strength is in the effort."]
   [:p "This is a version 1.0 release of this idea. You can check out the "[:a {:href "https://github.com/adam-james-v/svg-clj"} "github"] " to offer suggestions for improvements. You can even fork the repo and mess around with things yourself!"]
   [:p "Hercules is an idea I had while playing Supergiant's amazing rougelite Hades. I workout regularly, but want to add a stronger focus to mobility and overall body control and athletic strength. Playing games doesn't typically contribute to such goals, but with a bit of creativity, I figured I could give it an honest try."]
   [:p "Have fun and be safe! Your health is important and you can have fun being fit."]
   [:p "This project was designed and built by me, Adam James. You can see more of me and my work at the following places:"]
   [:ul
    [:li [:a {:href "https://github.com/adam-james-v"} "Github"]]
    [:li [:a {:href "https://www.youtube.com/c/AdamJames-tv"} "Youtube"]]
    [:li [:a {:href "https://twitch.tv/adam_james_tv"} "Twitch"]]
    [:li [:a {:href "https://twitter.com/RustyVermeer"} "Twitter"]] 
    [:li [:a {:href "https://www.instagram.com/adam.james.v/"} "Instagram"]]
    [:li [:a {:href "https://www.patreon.com/adam_james"} "Patreon"]]]])

(defn doc []
  [:<>
   patterns-component
   [:img {:src "hercules-logo.png"
          :alt "Hercules"
          :style {:width "100%" :padding-top "40px"}}]
   (apply boon-choices @state)
   controls
   blurb])

(defn mount [app]
  (rdom/render [app] (js/document.getElementById "root")))

(mount doc)
(defn ^:after-load re-render [] (mount doc))
(defonce go (do (mount doc) true))
