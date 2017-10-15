(ns hour-vt.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  (:import (org.apache.tika Tika)
           (java.text DecimalFormat)))

(defn is-summary-page? [page]
  (string/includes? page "TOTAL"))


(defn parse-num [num-text]
  (.parse (DecimalFormat. "######,####") num-text))

(defn split-page [text]
  (string/split text #"NO\. OF ELECTORS"))

(defn column-count [lines]
  (count (filter #(= % "VOTER  ") lines)))

(defn head-offset [lines]
  (or
    (ffirst
      (filter (fn [[idx line]]
                (string/includes? line "累積每時段投票人數摘要"))
        (map-indexed (fn [idx l]
                       [idx l])
                     lines)))
    0))

(defn page-dummy-row [lines]
  (if (some #(string/includes? % "PAGE:") lines)
    14
    13))

(def sections 6)

(defn parse-page [page]
  (let [lines (filter (comp not string/blank?)
                      (string/split-lines page))
        page-head-offset (inc (head-offset lines))
                         page-dummy-row (page-dummy-row lines)
        cols (column-count lines)
        station-count (/ (- (count lines) page-dummy-row (* 2 cols))
                         sections)
        parse-map {:time [page-head-offset 1]
                   :voter [(+ page-head-offset
                              1)
                           station-count]
                   :station-code [(+ 1 page-head-offset
                                     (* 2 station-count)
                                     (* 2 cols)
                                     2)
                                  station-count]
                   :station-name [(+ 1 page-head-offset
                                     (* 3 station-count)
                                     (* 2 cols)
                                     2)
                                  station-count]
                   :total-voter [(+ 1 page-head-offset
                                    (* 4 station-count)
                                    (* 2 cols)
                                    2)
                                 station-count]}]

    (into {}
      (map (fn [[k [offset sample]]]
             [k (take sample (drop offset lines))])
           parse-map))))

(defn per-station [{:keys [time voter station-code station-name total-voter]}]
  (let [time-slot (string/split (first time) #"\s+")]
    (map (fn [vr sc sn tv]
           {:voter (zipmap time-slot (string/split
                                       (string/trim vr)
                                       #"\s+"))
            :station-code (string/trim sc)
            :station-name (string/trim sn)
            :total-voter  (parse-num (string/trim tv))})
         voter station-code station-name total-voter)))

(defn pdf->text
  ""
  [x]
  (let [tika (doto (Tika.)
               (.setMaxStringLength Integer/MAX_VALUE))
        file (clojure.java.io/file x)]
    (.parseToString tika file)))

(defn station->csv-row [[code votes]]
  (let [all-votes (apply merge (map :voter votes))
        station (first votes)]
    (concat
      [code
       (:station-name station)
       (:total-voter station)]
      (map
        (fn [h]
          (parse-num (get all-votes h)))
        ["08:30"
         "09:30"
         "10:30"
         "11:30"
         "12:30"
         "13:30"
         "14:30"
         "15:30"
         "16:30"
         "17:30"
         "18:30"
         "19:30"
         "20:30"
         "21:30"
         "22:30"]))))

(defn table-data [file]
  (->> (pdf->text file)
       (split-page)
       (filter (comp not is-summary-page?))
       (mapcat (comp per-station parse-page))
       (group-by :station-code)
       (map station->csv-row)))

(defn -main [& args]
  "usage: lein run -m hour-vt.core"
  (with-open [writer (io/writer (second args))]
    (csv/write-csv writer
                   (table-data (first args)))))
