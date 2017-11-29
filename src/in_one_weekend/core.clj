(ns in-one-weekend.core
  (:require [clojure.java.io :as io :refer [writer]])
  (:gen-class))

(defn header [nx ny]
  (str "P3\n" nx " " ny "\n255\n"))

(defn int-color [f-color]
  (int (* 255.99 f-color)))

(defn body [nx ny]
  (apply str
         (for [j (range (- ny 1) -1 -1)
               i (range 0 nx)]
           (let [r (/ i (float nx))
                 g (/ j (float ny))
                 b 0.2]
             (let [[ir ig ib] (map int-color [r g b])]
               (str ir " " ig " " ib "\n"))))))

(defn -main [& args]
  (let [nx 200
        ny 100]
    (with-open [fout (io/writer "out.ppm")]
      (-> fout
          (.write (str (header nx ny) (body nx ny)))))))
