(ns witan.send.metadata
  (:require [clj-time.format :as f]
            [clj-time.core :as t]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [digest]
            [clojure.java.io :as io]))

(defn environment-metadata
  []
  (sh/sh "git" "fetch" "--tags")
  (let [git-commit-id (str/join ""
                                (drop-last
                                 (str/replace
                                  (:out (sh/sh "git"
                                               "log"
                                               "--format=\"%H\""
                                               "-n"
                                               "1"))
                                  "\"" "")))]
    {:model-version (str/join "" (drop-last (:out (sh/sh "git" "describe" "--abbrev=0" "--tags"))))
     :os-name (System/getProperty "os.name")
     :os-version (System/getProperty "os.version")
     :clj-version (clojure-version)
     :jvm-version (System/getProperty "java.vm.version")
     :git-branch (str/join ""
                           (drop-last
                            (:out (sh/sh "git"
                                         "rev-parse"
                                         "--symbolic-full-name"
                                         "--abbrev-ref"
                                         "HEAD"))))
     :git-commit-id (str/join "" (take 7 git-commit-id))
     :git-url (str "https://github.com/MastodonC/witan.send/commit/"
                   git-commit-id)}))

(defn time-metadata
  []
  {:date (f/unparse-local (f/formatter "YYYY-MM-dd") (t/today))
   :time (f/unparse-local (f/formatter-local "HH:mm:ss") (t/time-now))})

(defn file-input-md5s
  [{:keys [project-dir file-inputs]}]
  (into {} (for [[k v] file-inputs]
             [k (digest/md5 (io/file project-dir v))])))

(defn metadata
  [config]
  {:execution {:start (time-metadata)}
   :environment-metadata (environment-metadata)
   :file-inputs-md5s (file-input-md5s config)})

(defn merge-end-time
  [md]
  (merge-with merge
              md
              {:execution {:end (time-metadata)}}))
