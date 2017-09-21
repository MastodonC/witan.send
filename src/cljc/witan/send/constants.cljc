(ns witan.send.constants)

(def academic-years
  (range -4 (inc 21)))

(def min-academic-year (apply min academic-years))

(def max-academic-year (apply max academic-years))

(def settings
  [:EO :EYS :FEC :IMS :ISS :ISSR :MAP :MMS :MMSIB :MMSOB :MSS :MSSIB :MSSOB :MSSOP :MSSR :MU :MUOB :NMSS :NMSSR :OOE])

(def needs
  ;; Detailed needs
  #_[:ASD :CI :CL :HI :MLD :MSI :OTH :PD :PMLD :SEMH :SLCN :SLD :SP :SPLD :VI :UKN]
  ;; Broad need categories:
  [:CI :CL :OTH :SEMH :SP :UKN])
