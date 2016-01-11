(ns midi.core
  (:gen-class)
  (:import (javax.sound.midi MidiSystem))
  (:require [midi.sequencer :as s]
            [midi.scales :as sc]))

(defn midi-devs
  []
  (let [midi-infos (MidiSystem/getMidiDeviceInfo)]
    (zipmap (range (alength midi-infos))
            (map #(MidiSystem/getMidiDevice %) midi-infos))))

(defn start-seq
  [[midi-num bpm]]
  (.start (s/clock [(s/sequencer (get (midi-devs) midi-num) (sc/get-steps 16))] bpm)))

(defn -main
  [& args]
  (cond (empty? args) (println (midi-devs))
        :else (start-seq (map #(Long/valueOf %) args))))
