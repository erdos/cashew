(defproject cashew "0.1.0-SNAPSHOT"
  :description "computer algebra system in clojure"
  :url "https://github.com/erdos/cashew"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns cashew.core})
