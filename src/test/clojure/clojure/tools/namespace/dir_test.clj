(ns clojure.tools.namespace.dir-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.tools.namespace.test-helpers :as help]
            [clojure.tools.namespace.dir :as dir])
  (:import
   (java.io File)))

(defn- make-symbolic-link
  "Reflectively calls java.nio.file.Files/createSymbolicLink on two
  java.io.File arguments, to avoid a compile-time dependency on
  java.nio.file.Files. Returns a java.io.File."
  [^File link ^File target]
  (let [path-class (Class/forName "java.nio.file.Path")
        attr-class (Class/forName "java.nio.file.attribute.FileAttribute")
        attr-array (make-array attr-class 0)
        attr-array-class (.getClass attr-array)
        to-path (.getMethod java.io.File "toPath" (into-array Class []))
        to-file (.getMethod path-class "toFile" (into-array Class []))
        create-link (.getMethod (Class/forName "java.nio.file.Files")
                                "createSymbolicLink"
                                (into-array Class [path-class path-class attr-array-class]))
        link-path (.invoke to-path link (object-array 0))
        target-path (.invoke to-path target (object-array 0))
        link (.invoke create-link path-class (object-array [link-path target-path attr-array]))]
    (.invoke to-file link (object-array 0))))

;; Only run this test on Java 1.7+, where java.nio.file.Files is available.
(when (try (Class/forName "java.nio.file.Files")
           (catch ClassNotFoundException _ false))
  (deftest t-scan-by-canonical-path
    (let [dir (help/create-temp-dir "t-scan-by-canonical-path")
          main-clj (help/create-source dir 'example.main :clj '[example.one])
          one-cljc (help/create-source dir 'example.one :clj)
          other-dir (help/create-temp-dir "t-scan-by-canonical-path-other")
          link (File. other-dir "link")]
      (make-symbolic-link link dir)
      (is (= (::dir/files (dir/scan-dirs {} [dir]))
             (::dir/files (dir/scan-dirs {} [link])))))))

(deftest t-ignore-invalid-ns-path
  ;; example.one namespace is defined in cljc file and can be found
  ;; at two different paths on the classpath, in other case
  ;; the path doesn't match namespace.
  (let [dir (help/create-temp-dir "t-invalid-ns-path")
        main-clj (help/create-source dir 'example.main :clj '[example.one])
        one-cljc (help/create-source dir 'example.one :cljc)
        one-cljc-copy (help/create-copy
                       one-cljc
                       (io/file dir "public/js/out/example/one.cljc"))]
    (is (help/same-files?
         [main-clj one-cljc]
         (::dir/files (dir/scan-dirs {} [dir]))))))
