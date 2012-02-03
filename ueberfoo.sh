#!/bin/bash
java -cp ./classes:lib/clojure-1.3.0.jar clojure.main src/ueberfoo/cmdline.clj $@
