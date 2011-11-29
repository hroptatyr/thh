(deftimezone XEUR/timezone "Europe/Berlin")
(defvar XEUR/metronome "2000-01-01")

(defsession pre)
(defsession full)
(defsession late1)
(defsession late2)
(defsession post)
(defsession exer)
(defsession notify)


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
(deftrading-hours XEUR/Euro/old/reg
  "Bobl, Bund, Buxl, Schatz Futures, regular trading"
  :from "2000-01-01"
  :till "2005-11-20"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/Euro/reg
  "Bobl, Bund, Schatz Futures, regular trading"
  :from "2005-11-21"
  :open "08:00:00"
  :close "22:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/Euro/pre
  "Bobl, Bund, Buxl, Schatz Futures, pre trading"
  :from "2000-01-01"
  :open "07:30:00"
  :close "08:00:00"
  :timezone XEUR/timezone
  :session pre)

(deftrading-hours XEUR/Euro/late
  "Bobl, Bund, Buxl, Schatz Futures, late trading"
  :from "2011-01-01"
  :open "22:00:00"
  :close "22:30:00"
  :timezone XEUR/timezone
  :session post)

(deftrading-hours XEUR/Euro/last
  "Bobl, Bund, Buxl, Schatz Futures, last trading day, trading session"
  :from "2000-01-01"
  :open "08:00:00"
  :close "12:30:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/Euro/old2/last/notify
  "Bobl, Bund, Buxl, Schatz futures on the last trading day, notify session"
  :from "2005-01-01"
  :till "2005-12-31"
  :open "12:30:00"
  :close "22:30:00"
  :timezone XEUR/timezone
  :session notify)

(deftrading-hours XEUR/Euro/old1/last/notify
  "Bobl, Bund, Buxl, Schatz Futures, last trading day, notify session"
  :from "2000-01-01"
  :till "2004-12-31"
  :open "12:30:00"
  :close "19:30:00"
  :timezone XEUR/timezone
  :session notify)

(deftrading-hours XEUR/Euro/last/notify
  "Bobl, Bund, Buxl, Schatz Futures, last trading day, exercise session"
  :from "2006-01-01"
  :open "12:30:00"
  :close "20:00:00"
  :timezone XEUR/timezone
  :session notify)

(deftrading-hours XEUR/FGBX/old/reg
  :from "2005-09-09"
  :till "2005-11-20"
  :open "08:00:00"
  :close "19:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/FGBX/reg
  :from "2005-11-20"
  :open "08:00:00"
  :close "22:00:00"
  :timezone XEUR/timezone
  :session full)

(deftrading-hours XEUR/FGBX/pre
  :from "2005-09-09"
  :open "07:30:00"
  :close "08:00:00"
  :timezone XEUR/timezone
  :session pre)

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
;; no trading on this particular day but settlements
(defholiday/once whit-monday-2007
  :in-year 2007
  :function #'gregorian-whit-monday)

;; british hols plus GBP cash payments
(defholiday/yearly xmas/uk :in-lieu t :in dec :on 25)
(defholiday/yearly boxing-day/uk :in-lieu t :in dec :on 26)
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

;;; swiss holidays, no trading, no settlement, but clearing
;; 2000-08-01 -> swiss-national-day
;; 
;; 2001-01-02 -> st. berchtold
;; 2003-01-02 -> st. berchtold
;; 2004-01-02 -> st. berchtold
;; 2006-01-02 -> "
;; 2007-01-02 -> "
;; 
;; 2001-05-24 -> ascension
;; 2001-06-04 -> whit monday
;; 2003-05-29 -> ascension
;; 2003-06-09 -> whit monday
;; 2004-05-20 -> ascension
;; 2004-05-31 -> whit monday
;; 2005-05-05 -> ascension
;; 2005-05-16 -> whit monday
;; 2006-05-25 -> ascension
;; 2006-06-05 -> whit monday
;; 2007-05-17 -> ascension
;; 2007-05-28 -> whit-monday
;;
;; 2006-08-01 -> swiss-national-day
;; 2007-08-01 -> swiss-national-day
;;
;; 2000-01-03
;; 2000-06-01 -> Corpus Christi
;; 2000-06-12

;;; trading ceases early
;; 2001-04-12
;; 2001-05-23
;; 2001-12-24

;;; no settlement, but trading
;; 2002-05-09 -> ascension
;; 2002-05-20 -> whit monday
;; 
;; 2002-01-02 -> st. berchtold
;;
;; 2001-08-01 -> swiss-national-day
;; 2002-08-01 -> swiss-national-day
;; 2003-08-01 -> swiss-national-day
;; 2005-08-01 -> swiss-national-day

;;; no trading, no clearing (exer & stl) in swiss eqty and eqty index derivs
;; 2008-01-02 -> st. berchtold
;; 2008-05-12 -> whit monday
;; 2008-08-01 -> swiss-national-day

;;; no trading, no clearing in swiss eqty/eqty index and fixed income derivs
;;; also no CHF cash
;; 2009-01-02 -> st. berchtold
;; 2009-05-21 -> whit monday
;; 2009-06-01 -> ascension
;; 2010-05-13 -> whit monday
;; 2010-05-24 -> ascension

(defholiday/yearly st-berchtold :in jan :on 2)
(defholiday/yearly ascension :function #'gregorian-ascension)
(defholiday/yearly whit-monday :function #'gregorian-whit-monday)
(defholiday/yearly swiss-national-day
  :from "1994-01-01" ;; official
  :in aug :on 1)
(defholiday/yearly corpus-christi :function #'gregorian-corpus-christi)


;; actual rule sets
(defruleset XEUR/hols/all
  :metronome XEUR/metronome
  new-year easter xmas weekend labour-day)

(defruleset XEUR/hols/deriv
  :metronome XEUR/metronome
  XEUR/hols/all pre-xmas pre-new-year whit-monday-2007)

(defruleset XEUR/hols/uk
  :metronome XEUR/metronome
  easter weekend xmas/uk boxing-day/uk new-year/uk labour-day/uk
  XLON/summer-bh XLON/spring-bh XLON/may-day-bh
  prince-william-wedding)

;; rules for specific contracts
(defruleset XEUR/FGB?
  :metronome XEUR/metronome
  XEUR/hols/deriv
  XEUR/Euro/old/reg XEUR/Euro/reg XEUR/Euro/pre XEUR/Euro/late)

(defruleset XEUR/FGBS
  :metronome XEUR/metronome
  XEUR/FGB?)

(defruleset XEUR/FGBM
  :metronome XEUR/metronome
  XEUR/FGB?)

(defruleset XEUR/FGBL
  :metronome XEUR/metronome
  XEUR/FGB?)

(defruleset XEUR/FGBX
  :metronome XEUR/metronome
  XEUR/hols/deriv
  XEUR/FGBX/old/reg XEUR/FGBX/reg XEUR/FGBX/pre XEUR/Euro/late)
