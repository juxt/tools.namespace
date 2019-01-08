;; Copyright (c) Stuart Sierra, 2012. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license. You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Stuart Sierra"
      :doc "Track namespace dependencies and changes by monitoring
  file-modification timestamps"}
  clojure.tools.namespace.dir
  (:require [clojure.tools.namespace.file :as file]
            [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.parse :as parse]
            [clojure.tools.namespace.track :as track]
            [clojure.java.classpath :refer [classpath-directories]]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import (java.io File) (java.util.regex Pattern)))

(set! *warn-on-reflection* true)

(def ^:private ^:dynamic *path-mismatch-dirs*
  "Set of classpath directories found with extra source files whose
  paths do not match their ns declarations. Used to print warnings and
  filter out paths with non-canonical copies of source files, such as
  .cljc files copied into resources/public."
  nil)

(defn- add-mismatch-dir [^File dir]
  (when (thread-bound? #'*path-mismatch-dirs*)
    (set! *path-mismatch-dirs* (conj *path-mismatch-dirs* dir))))

(defn- remove-mismatch-dir [^File dir]
  (when (thread-bound? #'*path-mismatch-dirs*)
    (set! *path-mismatch-dirs* (disj *path-mismatch-dirs* dir))))

(defn- relative
  "Returns a java.io.File representing the path to file relative to
  dir. Like java.nio.file.Path.relativize() but compatible with Java
  1.6."
  ^File [^File dir ^File file]
  (loop [parts ()
         ^File current file]
    (cond
      (nil? current)  nil
      (= current dir) (apply io/file parts)
      :else           (recur (conj parts (.getName current))
                             (.getParentFile current)))))

(defn- warn-path-mismatch [^File dir ^File file]
  (let [relative-path (.getPath (relative dir file))
        ns-name (second (file/read-file-ns-decl file))]
    (binding [*err* *out*]
      (println (str "tools.namespace: ignoring directory " (.getPath dir)
                    "\n\tbecause the ns declaration " ns-name
                    "\n\tdoes not match the path " relative-path)))))

(defn- mismatch-path?
  "True if the directory has already been identified as containing
  files for which the ns declarations do not match the file paths, for
  example .cljc files copied into resources/public. Prints
  notification to *err*."
  [^File dir]
  (when (contains? *path-mismatch-dirs* dir)
    true))

(defn- path-matches-ns?
  "True if the namespace declaration of file matches its path relative
  to dir."
  [^File dir ^File file]
  (let [decl (file/read-file-ns-decl file)
        ns (parse/name-from-ns-decl decl)
        correct-path (when decl
                           (str (.getPath dir)
                                File/separator
                                (-> (name ns)
                                    (string/replace "-" "_")
                                    (string/replace "." File/separator)))) ]
    (when correct-path
      (.startsWith (.getPath file) correct-path))))

(defn- find-files
  "Finds source files for platform in directory for which the path
  matches the namespace declaration."
  [dirs platform]
  (->> dirs
       (map io/file)
       (map #(.getCanonicalFile ^File %))
       (filter #(.exists ^File %))
       (mapcat (fn [dir]
                 (let [sources (find/find-sources-in-dir dir platform)]
                   (if (mismatch-path? dir)
                     (if (every? #(path-matches-ns? dir %) sources)
                       (do (binding [*err* *out*]
                             (println (str "tools.namespace: directory " (.getPath ^File dir) " no longer contains mismatched"
                                           "\n\tnamespace declarations, no longer ignoring the directory.")))
                           (remove-mismatch-dir dir)
                           sources)
                       (do (binding [*err* *out*]
                             (println "tools.namespace: ignoring directory" (.getPath ^File dir)))
                           nil))
                     (filter (fn [source]
                               (if (path-matches-ns? dir source)
                                 source
                                 (do
                                   (warn-path-mismatch dir source)
                                   (add-mismatch-dir dir)
                                   nil)))
                             sources)))))
       (map #(.getCanonicalFile ^File %))))

(defn- modified-files [tracker files]
  (filter #(< (::time tracker 0) (.lastModified ^File %)) files))

(defn- deleted-files [tracker files]
  (set/difference (::files tracker #{}) (set files)))

(defn- update-files [tracker deleted modified {:keys [read-opts]}]
  (let [now (System/currentTimeMillis)]
    (-> tracker
        (update-in [::files] #(if % (apply disj % deleted) #{}))
        (file/remove-files deleted)
        (update-in [::files] into modified)
        (file/add-files modified read-opts)
        (assoc ::time now))))

(defn scan-files
  "Scans files to find those which have changed since the last time
  'scan-files' was run; updates the dependency tracker with
  new/changed/deleted files.

  files is the collection of files to scan.

  Optional third argument is map of options:

    :platform  Either clj (default) or cljs, both defined in
               clojure.tools.namespace.find, controls reader options for
               parsing files.

    :add-all?  If true, assumes all extant files are modified regardless
               of filesystem timestamps."
  {:added "0.3.0"}
  ([tracker files] (scan-files tracker files nil))
  ([tracker files {:keys [platform add-all?]}]
   (let [deleted (seq (deleted-files tracker files))
         modified (if add-all?
                    files
                    (seq (modified-files tracker files)))]
     (if (or deleted modified)
       (update-files tracker deleted modified platform)
       tracker))))

(defn scan-dirs
  "Scans directories for files which have changed since the last time
  'scan-dirs' or 'scan-files' was run; updates the dependency tracker
  with new/changed/deleted files.

  dirs is the collection of directories to scan, defaults to all
  directories on Clojure's classpath.

  Ignores directories containing files for which the namespace
  declaration does not match the path: prints a warning.

  Optional third argument is map of options:

    :platform  Either clj (default) or cljs, both defined in
               clojure.tools.namespace.find, controls file extensions
               and reader options.

    :add-all?  If true, assumes all extant files are modified regardless
               of filesystem timestamps."
  {:added "0.3.0"}
  ([tracker] (scan-dirs tracker nil nil))
  ([tracker dirs] (scan-dirs tracker dirs nil))
  ([tracker dirs {:keys [platform add-all?] :as options}]
   (let [ds (or (seq dirs) (classpath-directories))]
     (binding [*path-mismatch-dirs* (::path-mismatch-dirs tracker #{})]
       (-> tracker
           (scan-files (find-files ds platform) options)
           (assoc ::path-mismatch-dirs *path-mismatch-dirs*))))))

(defn scan
  "DEPRECATED: replaced by scan-dirs.

  Scans directories for Clojure (.clj, .cljc) source files which have
  changed since the last time 'scan' was run; update the dependency
  tracker with new/changed/deleted files.

  If no dirs given, defaults to all directories on the classpath."
  {:added "0.2.0"
   :deprecated "0.3.0"}
  [tracker & dirs]
  (scan-dirs tracker dirs {:platform find/clj}))

(defn scan-all
  "DEPRECATED: replaced by scan-dirs.

  Scans directories for all Clojure source files and updates the
  dependency tracker to reload files. If no dirs given, defaults to
  all directories on the classpath."
  {:added "0.2.0"
   :deprecated "0.3.0"}
  [tracker & dirs]
  (scan-dirs tracker dirs {:platform find/clj :add-all? true}))
