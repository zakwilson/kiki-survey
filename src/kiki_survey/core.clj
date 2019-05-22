(ns kiki-survey.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s]))

; This code it written in a way that favors clarity to a beginner over maximizing
; abstraction and avoiding repetition, though it does SOME of that. I'd be happy
; to help a reasonable number of people get set up with development environments
; where they can run the code themselves. Just in case, 5 is reasonable; 900 is not.

(def question-keys [:timestamp
                    :doctor-diagnosed
                    :self-diagnosed
                    :adhd-doctor
                    :adhd-details
                    :adhd-order
                    :adhd-remember
                    :adhd-procrastination
                    :adhd-fidget
                    :adhd-hyperfocus
                    :adhd-boring
                    :adhd-conversation
                    :adhd-waiting
                    :asd-doctor
                    :asd-multiple-choice
                    :asd-routine
                    :asd-hyperfocus
                    :asd-sensory
                    :asd-patterns
                    :asd-polite
                    :asd-extraversion
                    :asd-objects
                    :asd-interests
                    :asd-situations
                    :asd-conversation
                    :asd-eye-contact
                    :asd-details
                    :asd-appearance
                    :asd-emotion-expression
                    :asd-humor
                    :asd-intentions
                    :asd-interruptions
                    :asd-rambling
                    :asd-roleplaying
                    :asd-category
                    :asd-fidget
                    :asd-speech
                    :asd-same
                    :email
                    :age
                    :race
                    :gender
                    :healthcare
                    :employment
                    :education
                    :impovrished
                    :income
                    :relationship])

(defn read-file [f]
  (with-open [reader (io/reader f)]
    (->> (doall (csv/read-csv reader))
         (map (partial interleave question-keys))
         (map (partial apply hash-map)))))

(defn gender-match [entry word]
  (and (:gender entry)
       (s/includes? (:gender entry) word)))

(defn cis-male? [entry]
  (let [match (partial gender-match entry)]
    (and (match "Man")
         (not (match "Woman"))
         (not (match "Intersex"))
         (not (match "Transgender")))))

(defn cis-female? [entry]
  (let [match (partial gender-match entry)]
    (and (match "Woman")
         (not (match "Man")) ; note that includes? is case-sensitive
         (not (match "Intersex"))
         (not (match "Transgender")))))

(defn nonbinary? [entry]
  (let [match (partial gender-match entry)]
    (or (match "Transgender")
        (match "Intersex")
        (match "Nonbinary")
        (and (match "Man")
             (match "Woman")))))

(defn asd? [key survey-entry]
  (s/includes? (get survey-entry key)
               "ASD"))

(defn adhd? [key survey-entry]
  (s/includes? (get survey-entry key)
               "ADHD"))

(defn neurodivergent? [key survey-entry]
  (or (asd? key survey-entry)
      (adhd? key survey-entry)))

(defn ctd? [key survey-entry]
  (s/includes? (get survey-entry key)
               "EDS or Marfans"))

(defn chronic-pain? [key survey-entry]
  (s/includes? (get survey-entry key)
               "Chronic pain"))

(def doctor-asd? (partial asd? :doctor-diagnosed))
(def self-asd? (partial asd? :self-diagnosed))
(def doctor-adhd? (partial adhd? :doctor-diagnosed))
(def self-adhd? (partial adhd? :self-diagnosed))
(def doctor-ctd? (partial ctd? :doctor-diagnosed))
(def self-ctd? (partial ctd? :self-diagnosed))
(def self-chronic? (partial chronic-pain? :self-diagnosed))
(def doctor-chronic? (partial chronic-pain? :doctor-diagnosed))
(def doctor-neurodivergent? (partial neurodivergent? :doctor-diagnosed))

; these numbers are in quotes because to the computer, a string containing the
; character "2" is different from the number 2, and the form stored the former

(def agree ["1" "2"])
(def disagree ["3" "4"])

(def adhd-agree [:adhd-details
                 :adhd-order
                 :adhd-remember
                 :adhd-procrastination
                 :adhd-fidget
                 :adhd-hyperfocus
                 :adhd-boring
                 :adhd-conversation
                 :adhd-waiting])

(def asd-agree [:asd-multiple-choice
                :asd-routine
                :asd-hyperfocus
                :asd-sensory
                :asd-patterns
                :asd-polite
                :asd-objects
                :asd-interests
                :asd-situations
                :asd-conversation
                :asd-eye-contact
                :asd-details
                :asd-appearance
                :asd-emotion-expression
                :asd-humor
                :asd-rambling
                :asd-category
                :asd-fidget
                :asd-speech
                :asd-same])

(def asd-disagree [:asd-extraversion
                   :asd-intentions
                   :asd-interruptions
                   :asd-roleplaying])

; This function is a bit confusing. The call to some behaves just like include? does
; for strings by putting (get survey-entry item) in a set and testing that aginst agree
(defn score-adhd [survey-entry]
  (count (filter (fn [item]
                   (some #{(get survey-entry item)}
                         agree))
                 adhd-agree)))

(defn screened-adhd? [survey-entry]
  (>= (score-adhd survey-entry) 6))


(defn score-asd [survey-entry]
  (+ (count (filter (fn [item]
                      (some #{(get survey-entry item)}
                            agree))
                    asd-agree))
     (count (filter (fn [item]
                      (some #{(get survey-entry item)}
                            disagree))
                    asd-disagree))))

(defn screened-asd? [survey-entry]
  (>= (score-asd survey-entry) 15))


(defn report [entries]
  (let [total (count entries)
        percent (fn ([x] (* 100 (float (/ (count x) total))))
                  ([x y] (* 100 (float  (/ (count x) (count y))))))
        doctor-adhd (filter doctor-adhd? entries)
        doctor-asd (filter doctor-asd? entries)
        doctor-ctd (filter doctor-ctd? entries)
        doctor-chronic (filter (every-pred doctor-chronic?
                                    (complement doctor-ctd?))
                        entries)
        painless (filter (every-pred (complement doctor-chronic?)
                                     (complement doctor-ctd?))
                         entries)
        self-adhd (filter self-adhd? entries)
        self-asd (filter self-asd? entries)
        self-ctd (filter self-ctd? entries)
        screened-adhd (filter screened-adhd? entries)
        screened-asd (filter screened-asd? entries)
        any-dx-adhd (filter #(or (doctor-adhd? %) (self-adhd? %)) entries)
        any-dx-asd (filter #(or (doctor-asd? %) (self-asd? %)) entries)
        any-dx-ctd (filter #(or (doctor-ctd? %) (self-ctd? %)) entries)
        dr-adhd-screen-negative (filter (complement screened-adhd?)
                                               doctor-adhd)
        dr-asd-screen-negative (filter (complement screened-asd?)
                                           doctor-asd)        
        any-dx-adhd-screen-negative (filter (complement screened-adhd?)
                                            any-dx-adhd)
        any-dx-asd-screen-negative (filter (complement screened-asd?)
                                           any-dx-asd)
        not-dx-adhd (filter #(and ((complement doctor-adhd?) %)
                                  ((complement self-adhd?) %))
                            entries)
        not-dx-asd (filter #(and ((complement doctor-asd?) %)
                                 ((complement self-asd?) %))
                           entries)
        not-dx-ctd (filter #(and ((complement doctor-ctd?) %)
                                 ((complement self-ctd?) %))
                           entries)
        undiagnosed-adhd (filter screened-adhd? not-dx-adhd)
        undiagnosed-asd (filter screened-asd? not-dx-asd)
        all-adhd (filter #(or (doctor-adhd? %) (self-adhd? %) (screened-adhd? %))
                         entries)
        all-asd (filter #(or (doctor-asd? %) (self-asd? %) (screened-asd? %))
                        entries)
        ctd-and-adhd (filter #(or (doctor-adhd? %) (self-adhd? %) (screened-adhd? %))
                             any-dx-ctd)
        ctd-and-asd (filter #(or (doctor-asd? %) (self-asd? %) (screened-asd? %))
                            any-dx-ctd)
        ctd-and-neurodiverse (filter #(or (doctor-asd? %)
                                          (self-asd? %)
                                          (screened-asd? %)
                                          (doctor-adhd? %)
                                          (self-adhd? %)
                                          (screened-adhd? %))
                                     any-dx-ctd)
        not-ctd-and-adhd (filter #(or (doctor-adhd? %) (self-adhd? %) (screened-adhd? %))
                                 not-dx-ctd)
        not-ctd-and-asd (filter #(or (doctor-asd? %) (self-asd? %) (screened-asd? %))
                                not-dx-ctd)
        not-ctd-and-neurodiverse (filter #(or (doctor-asd? %)
                                          (self-asd? %)
                                          (screened-asd? %)
                                          (doctor-adhd? %)
                                          (self-adhd? %)
                                          (screened-adhd? %))
                                         not-dx-ctd)

        doctor-ctd-and-asd-only (filter (every-pred doctor-asd?
                                                    (complement doctor-adhd?))
                                        doctor-ctd)
        doctor-ctd-and-adhd-only (filter (every-pred doctor-adhd?
                                                     (complement doctor-asd?))
                                         doctor-ctd)
        doctor-ctd-and-both (filter (every-pred doctor-asd?
                                         doctor-adhd?)
                             doctor-ctd)
        doctor-ctd-and-neurotypical (filter (every-pred (complement doctor-adhd?)
                                                        (complement doctor-asd?))
                                            doctor-ctd)
        
        screened-ctd-and-asd-only (filter (every-pred screened-asd?
                                                      (complement screened-adhd?))
                                          doctor-ctd)
        screened-ctd-and-adhd-only (filter (every-pred screened-adhd?
                                                       (complement screened-asd?))
                                           doctor-ctd)
        screened-ctd-and-both (filter (every-pred screened-asd?
                                                  screened-adhd?)
                                      doctor-ctd)
        screened-ctd-and-neurotypical (filter (every-pred (complement screened-adhd?)
                                                          (complement screened-asd?))
                                              doctor-ctd)
        doctor-chronic-and-asd-only (filter (every-pred doctor-asd?
                                                        (complement doctor-adhd?))
                                            doctor-chronic)
        doctor-chronic-and-adhd-only (filter (every-pred doctor-adhd?
                                                         (complement doctor-asd?))
                                             doctor-chronic)
        doctor-chronic-and-both (filter (every-pred doctor-asd?
                                                    doctor-adhd?)
                                        doctor-chronic)
        doctor-chronic-and-neurotypical (filter (every-pred (complement doctor-adhd?)
                                                            (complement doctor-asd?))
                                                doctor-chronic)
        
        screened-chronic-and-asd-only (filter (every-pred screened-asd?
                                                          (complement screened-adhd?))
                                              doctor-chronic)
        screened-chronic-and-adhd-only (filter (every-pred screened-adhd?
                                                           (complement screened-asd?))
                                               doctor-chronic)
        screened-chronic-and-both (filter (every-pred screened-asd?
                                                      screened-adhd?)
                                          doctor-chronic)
        screened-chronic-and-neurotypical (filter (every-pred (complement screened-adhd?)
                                                              (complement screened-asd?))
                                                  doctor-chronic)
        doctor-painless-and-asd-only (filter (every-pred doctor-asd?
                                                         (complement doctor-adhd?))
                                             painless)
        doctor-painless-and-adhd-only (filter (every-pred doctor-adhd?
                                                          (complement doctor-asd?))
                                              painless)
        doctor-painless-and-both (filter (every-pred doctor-asd?
                                                     doctor-adhd?)
                                         painless)
        doctor-painless-and-neurotypical (filter (every-pred (complement doctor-adhd?)
                                                             (complement doctor-asd?))
                                                 painless)
        
        screened-painless-and-asd-only (filter (every-pred screened-asd?
                                                           (complement screened-adhd?))
                                               painless)
        screened-painless-and-adhd-only (filter (every-pred screened-adhd?
                                                            (complement screened-asd?))
                                                painless)
        screened-painless-and-both (filter (every-pred screened-asd?
                                                       screened-adhd?)
                                           painless)
        screened-painless-and-neurotypical (filter (every-pred (complement screened-adhd?)
                                                               (complement screened-asd?))
                                                   painless)]
    (println "Total entries: " total)
    (printf "Diagnosed with ADHD by a doctor: %d (%.0f%%)\n"
            (count doctor-adhd) (percent doctor-adhd))
    (printf "Diagnosed with ASD by a doctor: %d (%.0f%%)\n"
            (count  doctor-asd) (percent doctor-asd))
    (printf "Diagnosed with a connective tissue disorder by a doctor: %d (%.0f%%)\n"
            (count doctor-ctd) (percent doctor-ctd))
    (printf "Self-diagnosed with ADHD: %d (%.0f%%)\n"
            (count self-adhd) (percent self-adhd))
    (printf "Self-diagnosed with ASD: %d (%.0f%%)\n"
            (count self-asd) (percent self-asd))
    (printf "Diagnosed with ADHD by self and/or a doctor: %d (%.0f%%)\n"
            (count any-dx-adhd) (percent any-dx-adhd))
    (printf "Diagnosed with ASD by self and/or a doctor: %d (%.0f%%)\n"
            (count any-dx-asd) (percent any-dx-asd))
    (printf "Diagnosed with connective tissue disorder by self and/or a doctor: %d (%.0f%%)\n"
            (count any-dx-ctd) (percent any-dx-ctd))
    (printf "Screened positive for ADHD: %d (%.0f%%)\n"
            (count screened-adhd) (percent screened-adhd))
    (printf "Screened positive for ASD: %d (%.0f%%)\n"
            (count screened-asd) (percent screened-asd))
    (printf "Screened positive for ADHD with no diagnosis (self or doctor): %d (%.0f%%)\n"
            (count undiagnosed-adhd) (percent undiagnosed-adhd))
    (printf "Screened positive for ASD with no diagnosis (self or doctor): %d (%.0f%%)\n"
            (count undiagnosed-asd) (percent undiagnosed-asd))
    (printf "Screened negative for ADHD with a diagnosis from a doctor: %d (%.0f%%)\n"
            (count dr-adhd-screen-negative) (percent dr-adhd-screen-negative))
    (printf "Screened negative for ASD with a diagnosis from a doctor: %d (%.0f%%)\n"
            (count dr-asd-screen-negative) (percent dr-asd-screen-negative))
    (printf "Screened negative for ADHD with any diagnosis of ADHD (self or doctor): %d (%.0f%%)\n"
            (count any-dx-adhd-screen-negative) (percent any-dx-adhd-screen-negative))
    (printf "Screened negative for ASD with any diagnosis of ASD (self or doctor): %d (%.0f%%)\n"
            (count any-dx-asd-screen-negative) (percent any-dx-asd-screen-negative))
    (printf "Screened, self, or doctor diagnosed with ADHD: %d (%.0f%%)\n"
            (count all-adhd) (percent all-adhd))
    (printf "Screened, self, or doctor diagnosed with ASD: %d (%.0f%%)\n"
            (count all-asd) (percent all-asd))
    (printf "Any diagnosis of connective tissue disorder and any positive for ADHD: %d (%.0f%% of CTD) (%.0f%% of total)\n"
            (count ctd-and-adhd)
            (percent ctd-and-adhd any-dx-ctd)
            (percent ctd-and-adhd))
    (printf "Any diagnosis of connective tissue disorder and any positive for ASD: %d (%.0f%% of CTD) (%.0f%% of total)\n"
            (count ctd-and-asd)
            (percent ctd-and-asd any-dx-ctd)
            (percent ctd-and-asd))
    (printf "Any diagnosis of connective tissue disorder and any positive for neurodiversity: %d (%.0f%% of CTD) (%.0f%% of total)\n"
            (count ctd-and-neurodiverse)
            (percent ctd-and-neurodiverse any-dx-ctd)
            (percent ctd-and-neurodiverse))
    (printf "No diagnosis of connective tissue disorder and any positive for ADHD: %d (%.0f%% of non-CTD) (%.0f%% of total)\n"
            (count not-ctd-and-adhd)
            (percent not-ctd-and-adhd not-dx-ctd)
            (percent not-ctd-and-adhd))    
    (printf "No diagnosis of connective tissue disorder and any positive for ASD: %d (%.0f%% of non-CTD) (%.0f%% of total)\n"
            (count not-ctd-and-asd)
            (percent not-ctd-and-asd not-dx-ctd)
            (percent not-ctd-and-asd))
    (printf "No diagnosis of connective tissue disorder and any positive for neurodiversity: %d (%.0f%% of non-CTD) (%.0f%% of total)\n"
            (count not-ctd-and-neurodiverse)
            (percent not-ctd-and-neurodiverse not-dx-ctd)
            (percent not-ctd-and-neurodiverse))    

    (printf "\nCTD group (%d responses)\n" (count doctor-ctd))
    (printf "Diagnosed CTD and diagnosed ADHD: %d (%.0f%% of CTD)\n"
            (count doctor-ctd-and-adhd-only)
            (percent doctor-ctd-and-adhd-only doctor-ctd))
    (printf "Diagnosed CTD and diagnosed ASD: %d (%.0f%% of CTD)\n"
            (count doctor-ctd-and-asd-only)
            (percent doctor-ctd-and-asd-only doctor-ctd))
    (printf "Diagnosed CTD and diagnosed both ASD and ADHD: %d (%.0f%% of CTD)\n"
            (count doctor-ctd-and-both)
            (percent doctor-ctd-and-both doctor-ctd))
    (printf "Diagnosed CTD and no doctor diagnosis of neurodivergence: %d (%.0f%% of CTD)\n"
            (count doctor-ctd-and-neurotypical)
            (percent doctor-ctd-and-neurotypical doctor-ctd))
    (printf "Diagnosed CTD and screened ADHD: %d (%.0f%% of CTD)\n"
            (count screened-ctd-and-adhd-only)
            (percent screened-ctd-and-adhd-only doctor-ctd))
    (printf "Diagnosed CTD and screened ASD: %d (%.0f%% of CTD)\n"
            (count screened-ctd-and-asd-only)
            (percent screened-ctd-and-asd-only doctor-ctd))
    (printf "Diagnosed CTD and screened both ASD and ADHD: %d (%.0f%% of CTD)\n"
            (count screened-ctd-and-both)
            (percent screened-ctd-and-both doctor-ctd))
    (printf "Diagnosed CTD and no positive screen for neurodivergence: %d (%.0f%% of CTD)\n"
            (count screened-ctd-and-neurotypical)
            (percent screened-ctd-and-neurotypical doctor-ctd))

    (printf "\nChronic pain group (%d responses)\n" (count doctor-chronic))
    (printf "Diagnosed CHRONIC and diagnosed ADHD: %d (%.0f%% of CHRONIC)\n"
            (count doctor-chronic-and-adhd-only)
            (percent doctor-chronic-and-adhd-only doctor-chronic))
    (printf "Diagnosed CHRONIC and diagnosed ASD: %d (%.0f%% of CHRONIC)\n"
            (count doctor-chronic-and-asd-only)
            (percent doctor-chronic-and-asd-only doctor-chronic))
    (printf "Diagnosed CHRONIC and diagnosed both ASD and ADHD: %d (%.0f%% of CHRONIC)\n"
            (count doctor-chronic-and-both)
            (percent doctor-chronic-and-both doctor-chronic))
    (printf "Diagnosed CHRONIC and no doctor diagnosis of neurodivergence: %d (%.0f%% of CHRONIC)\n"
            (count doctor-chronic-and-neurotypical)
            (percent doctor-chronic-and-neurotypical doctor-chronic))
    (printf "Diagnosed CHRONIC and screened ADHD: %d (%.0f%% of CHRONIC)\n"
            (count screened-chronic-and-adhd-only)
            (percent screened-chronic-and-adhd-only doctor-chronic))
    (printf "Diagnosed CHRONIC and screened ASD: %d (%.0f%% of CHRONIC)\n"
            (count screened-chronic-and-asd-only)
            (percent screened-chronic-and-asd-only doctor-chronic))
    (printf "Diagnosed CHRONIC and screened both ASD and ADHD: %d (%.0f%% of CHRONIC)\n"
            (count screened-chronic-and-both)
            (percent screened-chronic-and-both doctor-chronic))
    (printf "Diagnosed CHRONIC and no positive screen for neurodivergence: %d (%.0f%% of CHRONIC)\n"
            (count screened-chronic-and-neurotypical)
            (percent screened-chronic-and-neurotypical doctor-chronic))

    (printf "\nPainless group (%d responses)\n" (count painless))
    (printf "Diagnosed PAINLESS and diagnosed ADHD: %d (%.0f%% of PAINLESS)\n"
            (count doctor-painless-and-adhd-only)
            (percent doctor-painless-and-adhd-only painless))
    (printf "Diagnosed PAINLESS and diagnosed ASD: %d (%.0f%% of PAINLESS)\n"
            (count doctor-painless-and-asd-only)
            (percent doctor-painless-and-asd-only painless))
    (printf "Diagnosed PAINLESS and diagnosed both ASD and ADHD: %d (%.0f%% of PAINLESS)\n"
            (count doctor-painless-and-both)
            (percent doctor-painless-and-both painless))
    (printf "Diagnosed PAINLESS and no doctor diagnosis of neurodivergence: %d (%.0f%% of PAINLESS)\n"
            (count doctor-painless-and-neurotypical)
            (percent doctor-painless-and-neurotypical painless))
    (printf "Diagnosed PAINLESS and screened ADHD: %d (%.0f%% of PAINLESS)\n"
            (count screened-painless-and-adhd-only)
            (percent screened-painless-and-adhd-only painless))
    (printf "Diagnosed PAINLESS and screened ASD: %d (%.0f%% of PAINLESS)\n"
            (count screened-painless-and-asd-only)
            (percent screened-painless-and-asd-only painless))
    (printf "Diagnosed PAINLESS and screened both ASD and ADHD: %d (%.0f%% of PAINLESS)\n"
            (count screened-painless-and-both)
            (percent screened-painless-and-both painless))
    (printf "Diagnosed PAINLESS and no positive screen for neurodivergence: %d (%.0f%% of PAINLESS)\n"
            (count screened-painless-and-neurotypical)
            (percent screened-painless-and-neurotypical painless))    
    ))


; check for doctor-diagnosed and adhd-doctor match

