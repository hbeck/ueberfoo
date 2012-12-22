(ns ueberfoo.cmdline.main-test
  (:use [ueberfoo.common common conversion date file filter maps strings queries])
  (:use [ueberfoo.cmdline parsing ds io transform])
  (:use [clojure.set :only [difference]])
  (:import [java.util Date])
  (:import [java.io File])
  (:use [clojure.test]))


