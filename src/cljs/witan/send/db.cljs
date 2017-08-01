(ns witan.send.db)


(def default-db
  {:name "re-frame"
   :academic-year 18
   :leaver-weights [50 10 1]
   :mover-alpha-weights [0 0]
   :mover-beta-weights [0 0]
   :joiner-alpha-weights [0 0]
   :joiner-beta-weights [0 0]
   :population {0 3051, -4 2869, 7 2417, 20 5671, 1 2944, 24 5876, -2 2606, 4 2747, -1 2807, 15 4736, 21 5848, 13 2527, 22 5885, -3 2792, 6 2471, 25 5625, 17 4390, 3 2760, 12 2300, 2 2879, 23 5898, 19 5206, 11 2330, 9 2292, 5 2634, 14 4497, 16 4540, 10 2296, 18 4752, -5 2983, 8 2464}
   :transitions {}

   :model-transitions {}})

