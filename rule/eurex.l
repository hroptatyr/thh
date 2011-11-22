(deftrading-hours th/eurex
  :from "2000-01-04" :till "2000-01-11"
  :open "08:00:00" :close "20:00:00")

(defholiday/yearly xmas :in dec :on 25)
(defholiday/yearly new-year :in jan :on 1)
(defholiday/weekly weekend/sat :on sat)
(defholiday/weekly weekend/sun :on sun)

(defruleset eurex-simple
  :metronome "2000-01-01"
  th/eurex new-year xmas weekend/sat weekend/sun)
