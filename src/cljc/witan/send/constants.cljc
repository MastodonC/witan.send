(ns witan.send.constants)

(def academic-years
  (range -5 (inc 22)))

(def min-academic-year (apply min academic-years))

(def max-academic-year (apply max academic-years))

(def settings
  [:CC :EO :FEC :IMS :IN :ISC :ISCR :ISS :ISSR :IT :MMS :MSS :OOE :PRU :MU])

(def needs
  [:SLD :ASD :MLD :PD :HI :M :SLCN :PMLD :SEMH :VI :OTH :SPLD :MSI])
