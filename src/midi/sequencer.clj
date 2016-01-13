(ns midi.sequencer
  (:require [midi.scales :as s]
            [clojure.core.async :as async])
  (:import (javax.sound.midi ShortMessage)))


(defprotocol Controlable
  (start [this])
  (stop [this])
  (main-loop [this])
  (set-bpm [this bpm]))

(defprotocol Stepable
  (step [this step-num]))

(defn bpm-to-ms [bpm]
  (/ (* 60 1000) bpm))

(defn send-off-all [midi-dev]
  (let [receiver (-> midi-dev .getReceiver)
        channel 0
        timestamp -1
        notes-off (ShortMessage. (+ 176 channel) 123 0)]
    (.send receiver notes-off timestamp)))

(defn send-message [note midi-dev]
  (let [receiver (-> midi-dev .getReceiver)
        channel 0
        timestamp -1
        note-on (ShortMessage. ShortMessage/NOTE_ON channel (:pitch note) (:velocity note))]
    (.send receiver note-on timestamp)))

(defrecord Sequencer [steps synth-chan transformer]
  Stepable
  (step [this timer]
    (let [step-num (mod timer (count steps))
          event (-> steps (nth step-num) (transformer))]
      (if (:note-on event)
        (send-message event synth-chan)
        (println "off")))))

(defrecord Clock [sequencers interval running]
  Controlable
  (start [this]
    (reset! running true)
    (main-loop this))

  (main-loop [this]
    (loop [count 0]
      (when @running (do
                       (doseq [sequencer sequencers]
                         (step sequencer count))
                       (Thread/sleep (/ @interval 2))
                       (doseq [sequencer sequencers]
                         (send-off-all (:synth-chan sequencer)))
                       (Thread/sleep (/ @interval 2))
                       (recur (inc count))))))

  (stop [this]
    (reset! running false)
    (async/go
      (Thread/sleep (* 2 interval))
      (doseq [sequencer sequencers]
          (stop (:synth-chan sequencer)))))

  (set-bpm [this bpm]
    (reset! interval (bpm-to-ms bpm))))

(defn sequencer
  ([synth-chan]
   (->Sequencer (s/get-steps 16)
                synth-chan identity))
  ([synth-chan steps]
   (.open synth-chan)
   (->Sequencer steps synth-chan identity)))

(defn clock [sequencers bpm]
  (->Clock sequencers (atom (bpm-to-ms bpm)) (atom true)))
