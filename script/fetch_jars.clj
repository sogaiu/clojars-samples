(ns fetch-jars
  (:require [babashka.fs :as fs]
            [babashka.http-client :as hc]
            [clojure.java.io :as cji]
            [conf :as cnf]))

(def default-n 10)

(def skip-urls
  (do
    (when (not (fs/exists? cnf/clojars-skip-urls))
      (spit cnf/clojars-skip-urls ""))
    (set (fs/read-all-lines (fs/file cnf/clojars-skip-urls)))))

(defn skip-url?
  [url]
  (or (skip-urls url)
      ;; XXX: could also remove from latest-release-jar-urls.txt file?
      (re-matches #"^.*lein-template.*" url)))

(defn parse-url
  [url]
  (when-let [[_ container-path jar-name]
             (re-matches #"^https://repo.clojars.org/(.*)/([^/]+\.jar)" url)]
    [container-path jar-name]))

(defn collect-urls
  [rdr n]
  (let [counter (atom n)
        urls (atom [])]
    ;; try to collect enough urls
    (doseq [url (line-seq rdr)
            :while (or (neg? n)
                       (pos? @counter))]
      (when (and (uri? (java.net.URI. url))
                 (not (skip-url? url)))
        (when-let [[container-path jar-name] (parse-url url)]
          (let [jar-path
                (str cnf/clojars-jars-root "/" container-path "/" jar-name)]
            ;; XXX: could fetching have failed without cleaning up the jar?
            (when-not (fs/exists? jar-path)
              (swap! urls conj url)
              (swap! counter dec))))))
    (when cnf/verbose (println "Collected" (count @urls) "urls"))
    urls))

(defn fetch-urls
  [rdr n]
  (let [urls (collect-urls rdr n)
        probs (atom [])]
    (if (empty? @urls)
      nil
      (let [start-time (System/currentTimeMillis)]
        (doseq [url @urls]
          (when-let [[container-path jar-name] (parse-url url)]
            (let [jar-dir (str cnf/clojars-jars-root "/" container-path)
                  jar-path (str jar-dir "/" jar-name)]
              (when-not (fs/exists? jar-path)
                (fs/create-dirs jar-dir))
              (try
                (cji/copy (:body (hc/get url {:as :stream}))
                          (cji/file jar-path))
                (catch Exception e
                  (println url)
                  (println e)
                  (println)
                  (swap! probs conj url))))))
        [(- (System/currentTimeMillis) start-time)
         probs]))))

(defn -main
  [& _args]
  (when (not (fs/exists? cnf/clojars-jars-root))
    (fs/create-dir cnf/clojars-jars-root))
  (with-open [rdr (cji/reader cnf/clojars-jar-list-path)]
    ;; n <= -1 means to fetch all remaining
    (let [n (if (empty? *command-line-args*)
              default-n
              (try
                (Integer/parseInt (first *command-line-args*))
                (catch Exception e
                  (println "Failed to parse as integer:"
                           (first *command-line-args*))
                  (System/exit 1))))]
      (when (zero? n)
        (println "Ok, if you say so.")
        (System/exit 0))
      (when-let [[duration probs] (fetch-urls rdr n)]
        (when (not (empty? @probs))
          (println "Encountered some problems")
          (println @probs))
        (println "Took:" duration "ms")))))
