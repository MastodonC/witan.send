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
   :transitions {[4 :SLCN-MMS :SLCN-MMS] 15, [11 :ASD-OOE :ASD-FEC] 1, [5 :SLCN-IMS :SLCN-IMS] 1, [7 :ASD-MSS :NON-SEND] 1, [-2 :NON-SEND :ASD-MMS] 3, [4 :SLD-MSS :SLD-MSS] 1, [8 :SLCN-MSS :SLCN-MMS] 1, [6 :ASD-MSS :ASD-MSS] 9, [10 :SEMH-MSS :SEMH-MSS] 5, [7 :SEMH-MSS :SEMH-OOE] 1, [13 :PMLD-MU :PMLD-MU] 1, [7 :SEMH-MMS :SEMH-MMS] 8, [12 :SLCN-IMS :SLCN-IMS] 1, [17 :HI-FEC :NON-SEND] 1, [6 :NON-SEND :ASD-MMS] 2, [2 :HI-MU :HI-MU] 1, [4 :MLD-MSS :MLD-MSS] 2, [7 :SLCN-MMS :SLCN-MMS] 22, [7 :NON-SEND :MLD-MMS] 2, [10 :MLD-ISS :MLD-ISS] 1, [5 :ASD-MMS :ASD-MMS] 9, [16 :SLCN-FEC :NON-SEND] 1, [11 :MLD-MSS :MLD-OOE] 1, [12 :VI-MMS :VI-MMS] 1, [15 :SLCN-FEC :NON-SEND] 1, [18 :MLD-ISC :NON-SEND] 1, [8 :SLCN-MMS :SLCN-MMS] 9, [12 :M-MSS :M-MSS] 1, [16 :SLCN-FEC :SLCN-FEC] 1, [11 :MLD-MSS :MLD-FEC] 6, [10 :HI-MSS :HI-MSS] 5, [14 :SLD-MSS :SLD-FEC] 2, [10 :SEMH-ISS :SEMH-ISS] 1, [-2 :NON-SEND :SLCN-MSS] 1, [9 :MLD-MMS :MLD-MMS] 9, [17 :NON-SEND :SLD-FEC] 1, [14 :ASD-FEC :NON-SEND] 1, [2 :HI-MMS :HI-MMS] 1, [11 :HI-MMS :HI-MMS] 2, [2 :MLD-MMS :MLD-MMS] 6, [15 :ASD-IT :ASD-FEC] 1, [6 :HI-IMS :HI-IMS] 1, [11 :ASD-MMS :ASD-MMS] 2, [3 :SLD-MMS :SLD-MMS] 1, [15 :PMLD-FEC :PMLD-FEC] 2, [7 :PMLD-MSS :PMLD-MSS] 3, [3 :SEMH-MMS :SEMH-MMS] 5, [1 :ASD-MMS :ASD-MMS] 13, [11 :SLCN-MMS :NON-SEND] 2, [0 :PD-MMS :PD-MMS] 1, [9 :M-EO :M-MSS] 1, [11 :ASD-MSS :ASD-FEC] 1, [-2 :NON-SEND :ASD-MU] 2, [5 :ASD-MU :ASD-MSS] 1, [8 :SLD-MSS :SLD-MSS] 2, [2 :NON-SEND :ASD-MMS] 2, [12 :HI-FEC :HI-FEC] 2, [15 :NON-SEND :ASD-OOE] 1, [10 :NON-SEND :SEMH-MSS] 1, [11 :PD-MMS :PD-MMS] 1, [-3 :ASD-MMS :ASD-MMS] 1, [14 :SLCN-FEC :SLCN-FEC] 4, [5 :MLD-MMS :MLD-MMS] 12, [11 :SLCN-MSS :SLCN-MSS] 1, [1 :SLCN-IMS :SLCN-IMS] 1, [1 :SLD-MSS :NON-SEND] 1, [11 :ASD-MU :ASD-FEC] 2, [14 :SEMH-FEC :SEMH-OOE] 1, [2 :PD-MSS :PD-MSS] 1, [11 :NON-SEND :MLD-FEC] 1, [8 :M-MSS :M-MSS] 1, [-1 :NON-SEND :SLCN-IMS] 1, [-1 :HI-MMS :HI-MMS] 1, [0 :HI-MMS :HI-MMS] 1, [11 :NON-SEND :SEMH-ISSR] 1, [11 :MLD-MMS :MLD-MMS] 3, [12 :ASD-FEC :ASD-FEC] 2, [-1 :ASD-MSS :ASD-MSS] 2, [10 :SEMH-MMS :SEMH-MMS] 6, [8 :ASD-ISS :ASD-ISS] 5, [9 :SLD-MSS :SLD-MSS] 2, [11 :SLCN-MSS :NON-SEND] 1, [15 :NON-SEND :MLD-FEC] 1, [11 :ASD-MU :ASD-MMS] 2, [11 :SLCN-MMS :SLCN-FEC] 1, [-2 :NON-SEND :SLCN-CC] 1, [1 :ASD-MU :ASD-MU] 7, [4 :NON-SEND :SEMH-PRU] 2, [5 :ASD-MSS :ASD-MSS] 4, [12 :ASD-MMS :ASD-MMS] 1, [3 :SLCN-MMS :SLCN-MMS] 15, [2 :HI-MSS :HI-MSS] 2, [3 :NON-SEND :SLCN-MMS] 3, [20 :SLCN-FEC :NON-SEND] 1, [4 :SPLD-MMS :SPLD-MMS] 1, [10 :SLCN-MSS :SLCN-MSS] 1, [5 :PD-MU :PD-MU] 1, [-3 :SLD-MSS :SLD-MSS] 1, [7 :NON-SEND :ASD-MMS] 1, [12 :SLD-MSS :SLD-MSS] 1, [11 :ASD-MMS :NON-SEND] 1, [11 :SEMH-MMS :SEMH-FEC] 1, [17 :MLD-FEC :MLD-FEC] 3, [14 :SLD-ISSR :SLD-ISSR] 1, [15 :PMLD-FEC :PMLD-OOE] 2, [9 :ASD-ISS :ASD-ISS] 1, [-1 :MLD-MSS :MLD-MSS] 1, [12 :MLD-FEC :MLD-FEC] 8, [-2 :MLD-MSS :MLD-MSS] 1, [-1 :ASD-CC :ASD-MU] 1, [15 :NON-SEND :ASD-FEC] 1, [8 :SLD-MSS :NON-SEND] 1, [10 :NON-SEND :SEMH-EO] 1, [11 :ASD-ISSR :ASD-ISS] 1, [17 :MLD-ISC :NON-SEND] 2, [2 :NON-SEND :SEMH-PRU] 1, [7 :NON-SEND :SPLD-MMS] 2, [1 :SLCN-MMS :SLCN-MU] 2, [2 :MLD-MSS :MLD-MSS] 2, [-1 :ASD-IMS :ASD-MSS] 1, [5 :SLCN-MMS :SLCN-MU] 1, [4 :NON-SEND :SPLD-MMS] 1, [6 :NON-SEND :SEMH-MMS] 2, [8 :NON-SEND :SEMH-PRU] 1, [-2 :NON-SEND :ASD-IN] 1, [0 :PMLD-MSS :PMLD-MSS] 2, [8 :SLCN-EO :SLCN-EO] 1, [-1 :NON-SEND :SPLD-MMS] 1, [11 :PMLD-MSS :PMLD-MSS] 1, [5 :M-IMS :M-IMS] 1, [8 :NON-SEND :SPLD-MMS] 1, [12 :SLCN-FEC :SLCN-OOE] 1, [11 :HI-IMS :HI-IMS] 1, [10 :SLD-MSS :SLD-MSS] 4, [8 :SPLD-ISS :SPLD-ISS] 1, [16 :SLD-FEC :SLD-FEC] 1, [0 :HI-MU :HI-MU] 1, [6 :PMLD-MSS :PMLD-MSS] 2, [13 :MLD-FEC :MLD-FEC] 3, [17 :SLD-FEC :SLD-FEC] 1, [7 :SEMH-MSS :SEMH-MSS] 1, [4 :SLCN-MMS :SLCN-MU] 1, [14 :OTH-FEC :NON-SEND] 1, [10 :ASD-MMS :ASD-ISS] 1, [15 :SLD-FEC :NON-SEND] 1, [8 :NON-SEND :SLCN-MMS] 1, [7 :PD-MMS :PD-MMS] 1, [10 :M-MMS :M-MMS] 1, [16 :ASD-ISC :ASD-ISC] 1, [14 :ASD-ISS :ASD-FEC] 4, [15 :SLCN-FEC :SLCN-FEC] 1, [10 :SEMH-ISSR :SEMH-EO] 1, [-1 :ASD-MU :ASD-MSS] 1, [7 :SLCN-IMS :SLCN-MMS] 1, [10 :ASD-IMS :ASD-IMS] 1, [11 :NON-SEND :SEMH-FEC] 1, [11 :SEMH-MSS :SEMH-OOE] 2, [-1 :NON-SEND :ASD-MSS] 4, [11 :ASD-MSS :ASD-MSS] 5, [15 :PD-FEC :PD-FEC] 1, [7 :SLCN-MSS :SLCN-MSS] 1, [0 :ASD-MU :ASD-MU] 6, [14 :ASD-ISSR :ASD-FEC] 1, [3 :MLD-MSS :MLD-MSS] 7, [6 :SLCN-MMS :SLCN-MSS] 1, [3 :NON-SEND :SEMH-PRU] 3, [12 :SLCN-MMS :SLCN-OOE] 1, [11 :M-MMS :M-MMS] 2, [8 :ASD-ISSR :ASD-ISSR] 1, [7 :ASD-MSS :ASD-MSS] 7, [18 :NON-SEND :SPLD-FEC] 1, [12 :ASD-ISSR :NON-SEND] 1, [12 :ASD-MMS :ASD-FEC] 1, [2 :ASD-MSS :ASD-MSS] 6, [9 :SEMH-ISSR :SEMH-ISSR] 1, [12 :SEMH-EO :SEMH-OOE] 2, [13 :SLD-FEC :SLD-FEC] 1, [-2 :NON-SEND :PD-MSS] 2, [7 :SEMH-EO :SEMH-EO] 1, [1 :SLCN-MMS :SLCN-MMS] 12, [1 :ASD-EO :ASD-EO] 2, [10 :SEMH-MSS :SEMH-MMS] 1, [13 :NON-SEND :SLCN-FEC] 1, [12 :SEMH-FEC :SEMH-FEC] 7, [4 :ASD-ISS :ASD-ISS] 1, [5 :MLD-MU :MLD-MU] 1, [5 :NON-SEND :SLCN-MMS] 3, [3 :SLD-MSS :SLD-MSS] 4, [3 :MLD-ISS :MLD-ISS] 1, [6 :SEMH-PRU :SEMH-PRU] 1, [10 :SLCN-MMS :SLCN-MMS] 10, [6 :HI-MMS :HI-MMS] 1, [11 :SEMH-PRU :NON-SEND] 1, [-1 :SLCN-CC :SLCN-MMS] 1, [15 :SLD-FEC :SLD-FEC] 1, [8 :SLCN-MMS :SLCN-MSS] 1, [11 :ASD-EO :NON-SEND] 1, [14 :PD-ISC :PD-FEC] 1, [12 :SEMH-EO :SEMH-EO] 1, [18 :MLD-FEC :MLD-OOE] 1, [6 :SEMH-PRU :SEMH-MMS] 2, [1 :NON-SEND :SLCN-MMS] 6, [13 :SEMH-ISSR :SEMH-ISSR] 1, [18 :ASD-ISC :ASD-ISC] 1, [12 :SEMH-OOE :SEMH-OOE] 1, [9 :MLD-MMS :NON-SEND] 1, [12 :SLCN-MMS :SLCN-MMS] 2, [1 :NON-SEND :PD-MU] 1, [17 :ASD-ISC :NON-SEND] 1, [10 :NON-SEND :SEMH-MMS] 1, [7 :SEMH-MSS :SEMH-ISSR] 1, [13 :SPLD-MMS :NON-SEND] 1, [4 :SEMH-MMS :SEMH-MMS] 3, [3 :M-MMS :M-MMS] 3, [8 :ASD-MMS :ASD-MMS] 5, [13 :MLD-MMS :MLD-OOE] 1, [-2 :NON-SEND :MLD-CC] 2, [9 :ASD-MU :ASD-MU] 5, [19 :PMLD-FEC :NON-SEND] 1, [0 :ASD-MU :ASD-MSS] 1, [12 :SLCN-FEC :SLCN-FEC] 10, [10 :SPLD-IMS :SPLD-IMS] 1, [13 :PD-IMS :PD-IMS] 1, [14 :SEMH-ISS :SEMH-FEC] 1, [9 :PMLD-MMS :PMLD-MMS] 1, [13 :PD-IMS :NON-SEND] 1, [11 :NON-SEND :MLD-MSS] 1, [12 :ASD-EO :ASD-EO] 1, [5 :ASD-MMS :NON-SEND] 1, [11 :MLD-MMS :NON-SEND] 3, [13 :SLCN-FEC :SLCN-OOE] 1, [14 :M-MMS :M-OOE] 1, [18 :NON-SEND :MLD-OOE] 1, [11 :MLD-MMS :MLD-FEC] 2, [12 :NON-SEND :MLD-FEC] 1, [14 :SEMH-FEC :SEMH-FEC] 1, [4 :NON-SEND :SEMH-MMS] 2, [8 :SEMH-ISSR :SEMH-ISSR] 1, [-1 :NON-SEND :ASD-MMS] 9, [13 :SLCN-FEC :SLCN-FEC] 1, [3 :ASD-MSS :ASD-MSS] 5, [13 :SEMH-MMS :NON-SEND] 2, [11 :SLCN-ISS :SLCN-IMS] 1, [7 :HI-OOE :HI-ISSR] 1, [3 :PD-MSS :PD-MSS] 2, [14 :SEMH-FEC :NON-SEND] 1, [-1 :NON-SEND :SLCN-MMS] 4, [8 :MLD-MSS :MLD-MSS] 7, [13 :SPLD-MMS :SPLD-MMS] 1, [6 :SLCN-IMS :SLCN-MSS] 1, [8 :SEMH-MSS :SEMH-MSS] 5, [2 :MLD-IMS :MLD-IMS] 1, [5 :ASD-ISS :ASD-ISS] 1, [12 :ASD-ISSR :ASD-OOE] 1, [14 :SPLD-FEC :NON-SEND] 1, [6 :SEMH-OOE :SEMH-ISSR] 1, [9 :MLD-MSS :MLD-MSS] 9, [4 :SLCN-MMS :NON-SEND] 1, [-1 :M-MSS :M-MSS] 1, [1 :MLD-IMS :MLD-IMS] 1, [15 :ASD-FEC :ASD-FEC] 2, [-1 :PD-IMS :PD-MU] 1, [7 :ASD-MMS :NON-SEND] 1, [5 :SLD-MSS :SLD-MSS] 3, [14 :ASD-FEC :ASD-FEC] 1, [13 :ASD-MSS :ASD-FEC] 1, [3 :NON-SEND :SPLD-MMS] 1, [10 :MLD-MMS :MLD-MSS] 1, [8 :HI-MSS :HI-MSS] 1, [17 :NON-SEND :ASD-FEC] 1, [4 :SLCN-ISS :SLCN-ISS] 1, [9 :SLCN-MMS :SLCN-MMS] 20, [0 :NON-SEND :ASD-MMS] 2, [17 :NON-SEND :MLD-FEC] 1, [16 :NON-SEND :HI-FEC] 1, [11 :NON-SEND :SEMH-IMS] 1, [13 :SLCN-MSS :SLCN-MSS] 1, [10 :ASD-MSS :ASD-MSS] 5, [2 :ASD-MU :ASD-MU] 3, [13 :ASD-ISSR :ASD-ISSR] 1, [13 :ASD-ISS :ASD-ISS] 3, [17 :PMLD-FEC :NON-SEND] 1, [6 :MLD-MMS :MLD-MMS] 6, [3 :ASD-MU :ASD-MU] 5, [8 :VI-MMS :VI-MMS] 1, [12 :ASD-ISSR :ASD-ISSR] 1, [14 :VI-MMS :NON-SEND] 1, [9 :ASD-MSS :ASD-ISS] 1, [0 :NON-SEND :SLCN-MMS] 6, [18 :SEMH-FEC :NON-SEND] 1, [4 :SLCN-MSS :SLCN-MSS] 1, [2 :PMLD-MSS :PMLD-MSS] 3, [16 :MLD-FEC :MLD-FEC] 3, [0 :NON-SEND :ASD-MSS] 1, [1 :PD-MSS :PD-MSS] 1, [13 :SLCN-MMS :SLCN-MMS] 1, [12 :SLCN-MMS :NON-SEND] 2, [3 :HI-MSS :HI-MSS] 1, [15 :MLD-FEC :MLD-FEC] 4, [13 :SLCN-IMS :NON-SEND] 1, [12 :SLCN-MMS :SLCN-FEC] 5, [10 :PD-MMS :PD-MMS] 2, [7 :SLCN-MMS :SLCN-MSS] 2, [-1 :NON-SEND :ASD-MU] 3, [4 :NON-SEND :ASD-MMS] 1, [2 :ASD-MMS :ASD-MSS] 1, [3 :PD-MU :PD-MU] 1, [0 :ASD-MMS :ASD-MMS] 7, [-1 :NON-SEND :MLD-MMS] 2, [5 :SEMH-MMS :SEMH-MMS] 3, [7 :SPLD-MMS :SPLD-MMS] 1, [9 :PD-MSS :PD-MSS] 2, [2 :SLD-MSS :SLD-MSS] 1, [12 :ASD-MSS :ASD-MSS] 5, [9 :NON-SEND :SEMH-MSS] 1, [11 :ASD-ISS :ASD-FEC] 1, [-2 :NON-SEND :ASD-CC] 5, [14 :ASD-ISS :ASD-ISC] 2, [5 :HI-MMS :HI-MMS] 2, [4 :NON-SEND :SLCN-MMS] 2, [5 :ASD-MU :ASD-MU] 1, [9 :HI-MSS :HI-MSS] 2, [6 :PD-MU :PD-MU] 1, [4 :ASD-MU :ASD-MSS] 1, [2 :SLCN-MMS :SLCN-MMS] 16, [3 :NON-SEND :MLD-MMS] 2, [11 :PD-MU :PD-MU] 1, [11 :SEMH-MMS :SEMH-OOE] 1, [12 :VI-MSS :VI-MSS] 1, [4 :ASD-MSS :ASD-MSS] 3, [10 :ASD-MU :ASD-MU] 1, [2 :VI-MMS :VI-MSS] 1, [15 :ASD-ISC :NON-SEND] 1, [3 :SPLD-MMS :SPLD-MMS] 1, [-1 :HI-CC :HI-CC] 1, [7 :PD-MSS :PD-MSS] 1, [13 :SPLD-FEC :SPLD-FEC] 2, [0 :SLD-MSS :SLD-MSS] 2, [11 :ASD-ISS :ASD-ISS] 2, [5 :SLCN-MMS :SLCN-MMS] 12, [5 :SLCN-PRU :SLCN-PRU] 1, [5 :NON-SEND :ASD-MMS] 1, [13 :ASD-FEC :NON-SEND] 1, [1 :MLD-MMS :MLD-MMS] 3, [11 :SLCN-MSS :SLCN-FEC] 2, [5 :MLD-MSS :MLD-MSS] 5, [11 :SEMH-MSS :SEMH-FEC] 3, [4 :ASD-MU :ASD-MU] 3, [3 :NON-SEND :SEMH-MMS] 3, [4 :NON-SEND :MLD-MMS] 1, [0 :NON-SEND :SEMH-MMS] 2, [14 :ASD-MSS :ASD-FEC] 2, [2 :M-MMS :M-MMS] 4, [8 :MLD-MSS :MLD-EO] 1, [13 :MLD-MSS :MLD-MSS] 1, [6 :PD-MMS :PD-MMS] 1, [15 :NON-SEND :SEMH-FEC] 1, [6 :SLD-MSS :SLD-MSS] 2, [9 :SPLD-MSS :SPLD-MSS] 1, [3 :ASD-MU :NON-SEND] 1, [8 :SEMH-MSS :SEMH-MMS] 1, [9 :ASD-MMS :ASD-MMS] 1, [7 :HI-MMS :HI-MMS] 3, [11 :SPLD-MMS :SPLD-MMS] 1, [11 :SEMH-MSS :NON-SEND] 1, [7 :HI-MU :HI-MMS] 1, [8 :ASD-MSS :ASD-MSS] 7, [9 :ASD-MSS :ASD-MSS] 3, [10 :PD-IMS :PD-IMS] 1, [-1 :NON-SEND :SLCN-MSS] 1, [9 :SEMH-MSS :SEMH-MSS] 5, [3 :SLCN-MU :SLCN-MU] 1, [7 :MLD-MMS :MLD-MMS] 12, [6 :NON-SEND :SPLD-MMS] 1, [20 :PMLD-FEC :NON-SEND] 1, [8 :SPLD-MMS :SPLD-MMS] 3, [13 :SLD-MSS :SLD-FEC] 1, [11 :MLD-MMS :MLD-OOE] 1, [12 :SEMH-FEC :NON-SEND] 3, [1 :NON-SEND :ASD-MMS] 2, [6 :SPLD-MMS :SPLD-MMS] 2, [4 :PD-MU :PD-MSS] 1, [9 :SEMH-MSS :SEMH-ISSR] 1, [9 :PD-MMS :PD-MMS] 3, [-2 :NON-SEND :ASD-EO] 1, [3 :HI-IMS :HI-IMS] 1, [8 :NON-SEND :SEMH-OOE] 1, [6 :SEMH-MMS :SEMH-ISS] 1, [3 :SLCN-MMS :SLCN-MU] 1, [6 :ASD-MMS :ASD-MSS] 1, [5 :SLCN-MSS :SLCN-MSS] 1, [10 :PD-ISSR :PD-ISSR] 1, [8 :SLCN-MU :SLCN-MU] 1, [11 :SLD-MSS :SLD-MSS] 3, [-1 :PD-IN :PD-MU] 1, [11 :SPLD-MMS :NON-SEND] 1, [18 :ASD-FEC :ASD-FEC] 1, [6 :SLCN-MMS :SLCN-MMS] 9, [1 :PMLD-MSS :NON-SEND] 1, [10 :SEMH-ISSR :SEMH-ISSR] 3, [-2 :NON-SEND :PD-MMS] 1, [-1 :NON-SEND :SLCN-ISS] 1, [13 :SLCN-OOE :SLCN-OOE] 1, [1 :M-MSS :M-MSS] 1, [8 :ASD-MU :ASD-MU] 3, [5 :NON-SEND :SEMH-PRU] 2, [9 :NON-SEND :MLD-MMS] 1, [11 :SEMH-MMS :NON-SEND] 3, [4 :NON-SEND :SPLD-ISS] 1, [14 :HI-FEC :NON-SEND] 1, [1 :NON-SEND :SEMH-MMS] 2, [11 :SEMH-ISSR :SEMH-OOE] 1, [1 :NON-SEND :PMLD-MSS] 1, [12 :MLD-MSS :MLD-MSS] 6, [6 :PD-ISS :PD-ISS] 1, [12 :SPLD-MMS :SPLD-MMS] 2, [9 :MLD-MMS :MLD-MSS] 1, [11 :NON-SEND :SEMH-OOE] 1, [1 :MLD-MMS :NON-SEND] 1, [0 :MLD-MMS :MLD-MMS] 2, [4 :HI-IMS :HI-IMS] 1, [5 :PD-MMS :PD-MMS] 1, [12 :VI-MSS :VI-EO] 1, [14 :PD-MMS :NON-SEND] 1, [2 :NON-SEND :MLD-MMS] 3, [4 :M-MMS :M-MMS] 2, [-1 :HI-MU :HI-MU] 1, [2 :SEMH-IT :SEMH-PRU] 1, [12 :SEMH-MMS :SEMH-MMS] 2, [7 :ASD-MU :ASD-MU] 4, [17 :PMLD-FEC :PMLD-FEC] 1, [2 :PD-MMS :PD-MMS] 1, [8 :PD-MSS :PD-ISSR] 1, [3 :SEMH-MU :SEMH-MU] 1, [11 :SLCN-MMS :SLCN-MMS] 7, [-2 :NON-SEND :MLD-IN] 1, [1 :ASD-MSS :ASD-MSS] 5, [9 :VI-MMS :VI-MMS] 3, [6 :ASD-MMS :ASD-MU] 3, [13 :SLCN-FEC :NON-SEND] 3, [19 :MLD-FEC :MLD-FEC] 1, [6 :M-MSS :M-MSS] 2, [10 :SLD-ISS :SLD-ISS] 1, [11 :SLCN-MSS :SLCN-OOE] 1, [10 :MLD-MSS :MLD-MSS] 11, [10 :SEMH-MMS :SEMH-IMS] 1, [13 :ASD-FEC :ASD-FEC] 1, [6 :ASD-MU :ASD-MSS] 1, [12 :SEMH-MMS :NON-SEND] 2, [2 :VI-IMS :VI-IMS] 1, [10 :SEMH-MSS :SEMH-FEC] 1, [9 :SLCN-ISSR :SLCN-ISSR] 1, [4 :ASD-MMS :ASD-MMS] 9, [10 :SLCN-ISS :SLCN-ISS] 1, [13 :SLCN-ISS :SLCN-ISS] 1, [9 :SLCN-MSS :SLCN-MSS] 3, [6 :MLD-MMS :MLD-MSS] 4, [4 :MLD-MMS :MLD-MMS] 6, [9 :SEMH-MMS :SEMH-MMS] 8, [-1 :ASD-MMS :ASD-MMS] 3, [14 :MLD-FEC :MLD-FEC] 4, [8 :MLD-MMS :MLD-MMS] 8, [14 :SLCN-MMS :NON-SEND] 1, [6 :NON-SEND :SEMH-MSS] 2, [11 :SEMH-MMS :SEMH-MMS] 1, [13 :MLD-MMS :NON-SEND] 2, [6 :NON-SEND :MLD-MMS] 1, [2 :SEMH-MMS :SEMH-MMS] 3, [5 :M-MMS :M-MMS] 2, [17 :SLCN-FEC :NON-SEND] 1, [6 :SEMH-MMS :SEMH-MMS] 5, [9 :SEMH-MMS :SEMH-MSS] 1, [5 :PD-MSS :PD-MSS] 1, [9 :NON-SEND :SPLD-MMS] 1, [12 :ASD-ISS :ASD-FEC] 1, [0 :ASD-MSS :ASD-MSS] 7, [9 :SPLD-MMS :SPLD-MMS] 1, [9 :OTH-MMS :OTH-MMS] 1, [16 :HI-FEC :HI-FEC] 1, [14 :MLD-ISS :MLD-OOE] 1, [9 :MLD-MSS :MLD-OOE] 1, [15 :SEMH-FEC :SEMH-FEC] 1, [7 :ASD-MMS :ASD-MMS] 5, [8 :SEMH-MMS :SEMH-MMS] 7, [11 :SLCN-ISSR :SLCN-ISSR] 1, [2 :SLCN-MMS :SLCN-MU] 1, [2 :MLD-MMS :MLD-MSS] 1, [-2 :NON-SEND :MLD-MSS] 1, [-2 :NON-SEND :ASD-MSS] 7, [7 :MSI-MSS :MSI-MSS] 1, [2 :NON-SEND :SEMH-MMS] 2, [9 :ASD-IMS :ASD-ISS] 1, [5 :NON-SEND :ASD-IMS] 1, [14 :ASD-MMS :ASD-MMS] 1, [3 :OTH-MMS :OTH-MMS] 2, [10 :ASD-MMS :ASD-MMS] 2, [15 :SPLD-FEC :NON-SEND] 1, [6 :ASD-MMS :ASD-MMS] 3, [3 :M-MSS :M-MSS] 1, [0 :SLCN-IMS :SLCN-IMS] 1, [7 :MLD-MSS :MLD-MSS] 5, [2 :SLCN-IMS :SLCN-IMS] 1, [2 :PD-MU :PD-MU] 1, [6 :ASD-MU :ASD-MU] 1, [13 :SEMH-FEC :NON-SEND] 4, [12 :SPLD-MMS :NON-SEND] 2, [11 :MLD-MSS :MLD-MSS] 3, [10 :SEMH-MSS :NON-SEND] 1, [2 :NON-SEND :SLCN-MMS] 5, [13 :SPLD-FEC :NON-SEND] 1, [-1 :PD-MMS :PD-MMS] 1, [16 :ASD-ISC :NON-SEND] 2, [7 :M-MMS :M-MMS] 2, [16 :MLD-FEC :NON-SEND] 1, [9 :SLCN-ISS :SLCN-ISS] 1, [12 :SPLD-FEC :NON-SEND] 2, [0 :SLCN-MMS :SLCN-MU] 2, [5 :SEMH-PRU :SEMH-PRU] 3, [12 :SLCN-FEC :NON-SEND] 1, [8 :SEMH-MMS :SEMH-MSS] 3, [4 :PD-MSS :PD-MSS] 1, [12 :M-MMS :M-MMS] 1, [9 :PMLD-MSS :PMLD-MSS] 1, [18 :MLD-FEC :NON-SEND] 3, [10 :SPLD-MMS :SPLD-MMS] 2, [13 :SLCN-MMS :NON-SEND] 3, [1 :MLD-MSS :MLD-MSS] 2, [-1 :SLCN-MMS :SLCN-MMS] 2, [15 :ASD-ISC :ASD-ISC] 1, [-3 :NON-SEND :HI-MSS] 1, [3 :NON-SEND :SPLD-ISS] 1, [5 :ASD-IMS :ASD-IMS] 1, [12 :SEMH-FEC :SEMH-OOE] 3, [1 :ASD-MMS :ASD-MU] 1, [16 :ASD-FEC :ASD-FEC] 2, [-2 :PD-IN :PD-IN] 2, [13 :PD-MMS :PD-MMS] 1, [4 :PD-MU :PD-MU] 1, [7 :HI-ISS :HI-ISS] 2, [3 :ASD-MMS :ASD-MMS] 10, [14 :SLCN-MSS :SLCN-FEC] 2, [6 :SLCN-MSS :SLCN-MSS] 2, [2 :ASD-MMS :ASD-MMS] 5, [6 :ASD-ISS :ASD-ISS] 1, [1 :NON-SEND :MLD-MMS] 2, [13 :NON-SEND :MLD-FEC] 1, [0 :SLCN-MMS :SLCN-MMS] 8, [12 :MLD-MMS :MLD-FEC] 2, [2 :SEMH-PRU :SEMH-PRU] 1, [12 :ASD-ISS :ASD-ISS] 5, [5 :NON-SEND :MLD-MMS] 2, [7 :SLD-MSS :SLD-MSS] 3, [16 :VI-FEC :VI-FEC] 1, [-1 :ASD-IMS :ASD-MMS] 1, [19 :ASD-FEC :NON-SEND] 1, [8 :SEMH-MSS :SEMH-ISSR] 1, [18 :PMLD-FEC :NON-SEND] 1, [13 :ASD-ISS :NON-SEND] 2, [6 :MLD-MSS :MLD-MSS] 6, [-1 :ASD-CC :ASD-MSS] 2, [12 :PMLD-MSS :PMLD-MSS] 1, [12 :PD-MMS :PD-MMS] 1, [10 :MLD-MMS :MLD-MMS] 13, [13 :MLD-FEC :NON-SEND] 4, [9 :SLCN-MMS :NON-SEND] 3, [16 :PMLD-FEC :PMLD-FEC] 1, [0 :MLD-IN :MLD-MMS] 1, [22 :MLD-FEC :NON-SEND] 1, [10 :ASD-ISSR :ASD-ISSR] 1, [10 :ASD-MMS :NON-SEND] 1, [6 :MLD-MMS :NON-SEND] 1, [7 :M-MSS :M-MSS] 1, [3 :PMLD-MSS :PMLD-MSS] 1, [4 :PMLD-MSS :PMLD-MSS] 2, [3 :MLD-MMS :MLD-MMS] 3}})

