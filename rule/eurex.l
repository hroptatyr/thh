(deftimezone XEUR/timezone "Europe/Berlin")
(defvar XEUR/metronome "1999-01-01")

(defsession full)
(defsession late1)
(defsession late2)
(defsession post)
(defsession exer)


;; CONF futures
(deftrading-hours XEUR/CONF/reg
  "CONF Futures, regular trading"
  :open "08:30:00"
  :close "17:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/CONF/late
  "CONF Futures, late trading"
  :open "17:00:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/CONF/last
  "CONF Futures, last trading day, trading session"
  :open "08:30:00"
  :close "12:30:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/CONF/last/exer
  "CONF Futures, last trading day, exercise session"
  :open "12:30:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session exer)


;; Bobl, Bund, Buxl, Schatz futures (FGBS, FGBM, FGBL, FGBX)
(deftrading-hours XEUR/Euro/reg
  "Bobl, Bund, Buxl, Schatz Futures, regular trading"
  :open "08:00:00"
  :close "22:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/Euro/late
  "Bobl, Bund, Buxl, Schatz Futures, late trading"
  :open "22:00:00"
  :close "22:30:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/Euro/last
  "Bobl, Bund, Buxl, Schatz Futures, last trading day, trading session"
  :open "08:00:00"
  :close "12:30:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/Euro/last/exer
  "Bobl, Bund, Buxl, Schatz Futures, last trading day, exercise session"
  :open "12:30:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session exer)


;; 1m EONIA futures
(deftrading-hours XEUR/FEO1/reg
  "FEO1 regular trading"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/FEO1/late
  "FEO1 late trading"
  :open "19:00:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/FEO1/last
  "FEO1 last trading day, trading session"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)


;; One-Year Mid Curve Options on Three-Month EURIBOR Futures (OEUH, OEUM, OEUU, OEUZ)
(deftrading-hours XEUR/OEU/reg
  "OEU futopts regular trading"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/OEU/late
  "OEU futopts late trading"
  :open "19:00:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/OEU/last
  "OEU last trading day, trading session"
  :open "08:00:00"
  :close "11:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/OEU/last/exer
  "OEU last trading day, exercise session"
  :open "11:00:00"
  :close "11:45:00"
  :timezone XEUR/timezone
  :session exer)


;; futopts Euro* (OGBM OGBL OGBS)
(deftrading-hours XEUR/EuroFutopt/reg
  "Bobl, Bund, Schatz futopts, regular trading"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/EuroFutopt/late1
  "Bobl, Bund, Buxl, Schatz futopts, late trading"
  :open "19:00:00"
  :close "19:15:00"
  :timezone XEUR/timezone
  :session late1)

(deftrading-hours XEUR/EuroFutopt/late2
  "Bobl, Bund, Buxl, Schatz futopts, late trading, session 2"
  :open "19:15:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/EuroFutopt/last
  "Bobl, Bund, Buxl, Schatz futopts, last trading day, trading session"
  :open "08:00:00"
  :close "17:15:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/EuroFutopt/late2
  "Bobl, Bund, Buxl, Schatz futopts, last trading day, late trading session 2"
  :open "17:15:00"
  :close "17:45:00"
  :timezone XEUR/timezone
  :session late2)

(deftrading-hours XEUR/EuroFutopt/last/exer
  "Bobl, Bund, Buxl, Schatz futopts, last trading day, exercise session"
  :open "17:45:00"
  :close "18:00:00"
  :timezone XEUR/timezone
  :session exer)


;; options on 3m EURIBOR futures (OEU3)
(deftrading-hours XEUR/OEU3/reg
  "euribor 3m futopts, regular trading"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/OEU3/late
  "euribor 3m futopts, late trading"
  :open "19:00:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/OEU3/last
  "euribor 3m futopts, last trading day, regular trading"
  :open "08:00:00"
  :close "11:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/OEU3/exer
  "euribor 3m futopts, last trading day, exercise"
  :open "11:00:00"
  :close "11:45:00"
  :timezone XEUR/timezone
  :session exer)


;; euro-btp futures (FBTS, FBTM, FBTP)
(deftrading-hours XEUR/EBTP/reg
  "Euro-BTP futures, regular trading"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/EBTP/late
  "Euro-BTP futures, late trading"
  :open "19:00:00"
  :close "19:30:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/EBTP/last
  "Euro-BTP futures, last trading day, regular trading"
  :open "08:00:00"
  :close "12:30:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/EBTP/last/exer
  "Euro-BTP futures, last trading day, exercise"
  :open "12:30:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session exer)


;; euribor-3m futures
(deftrading-hours XEUR/FEU3/reg
  "EURIBOR 3m futures, regular trading"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/FEU3/late
  "EURIBOR 3m futures, late trading"
  :open "19:00:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/FEU3/last
  "EURIBOR 3m futures, last trading day"
  :open "08:00:00"
  :close "11:00:00"
  :timezone XEUR/timezone
  :session full)


;; all-eurex holidays
(defholiday/weekly weekend :on sat :for 2)
(defholiday/yearly easter :function #'gregorian-good-friday :for 4)
(defholiday/yearly xmas :in dec :on 25 :for 2)
(defholiday/yearly new-year :in jan :on 1)
(defholiday/yearly labour-day :in may :on 1)

;; deriv holidays
(defholiday/yearly pre-xmas :in dec :on 24)
(defholiday/yearly pre-new-year :in dec :on 31)

;; british hols plus GBP cash payments
(defholiday xmas/uk :in-lieu xmas)
(defholiday new-year/uk :in-lieu new-year)
(defholiday labour-day/uk :in-lieu labour-day)

(defholiday/yearly XLON/summer-bh :in aug :on mon :which last)
(defholiday/yearly XLON/spring-bh :in may :on mon :which last)
(defholiday/yearly XLON/may-day-bh :in may :on mon :which 1st)
(defholiday/once prince-william-wedding :on "2011-04-29")

;; austrian holidays
(defholiday/yearly epiphany :in jan :on 6)
(defholiday/yearly austrian-national :in oct :on 26)
(defholiday/yearly all-saints :in nov :on 1)
(defholiday/yearly immaculate-conception :in dec :on 8)


;; actual rule sets
(defruleset XEUR/hols/all
  :metronome XEUR/metronome
  new-year easter xmas weekend labour-day)

(defruleset XEUR/hols/deriv
  :metronome XEUR/metronome
  XEUR/hols/all pre-xmas pre-new-year)

(defruleset XEUR/hols/uk
  :metronome XEUR/metronome
  XEUR/hols/all xmas/uk new-year/uk labour-day/uk
  XLON/summer-bh XLON/spring-bh XLON/may-day-bh
  prince-william-wedding)

