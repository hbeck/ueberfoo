#!/bin/bash
java -cp ./classes:lib/clojure-1.4.0.jar clojure.main src/ueberfoo/cmdline/main.clj $@
