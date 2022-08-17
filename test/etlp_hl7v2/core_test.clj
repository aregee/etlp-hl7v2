(ns etlp-hl7v2.core-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [etlp-hl7v2.core :refer [hl7-xform indexed-map is-valid-hl7?
                                     not-empty? parse parse-segment parse-value
                                     separators split-by logger]]
            [etlp-hl7v2.model.core :as model]
            [flatland.ordered.map :refer [ordered-map]]
            [matcho.core :refer [match]]))


(def valid-msg
  "MSH|^~\\&|AccMgr|1|||20151015200643||ADT^A01|599102|P|2.3|foo||
EVN|A01|20151010045502|||||
PID|1|010107111^^^MS4^PN^|1609220^^^MS4^MR^001|1609220^^^MS4^MR^001|BARRETT^JEAN^SANDY^^||19440823|F||C|STRAWBERRY AVE^FOUR OAKS LODGE^ALBUKERKA^CA^98765^USA^^||(111)222-3333||ENG|W|CHR|111155555550^^^MS4001^AN^001|123-22-1111||||OKLAHOMA|||||||N
PV1|1|I|PREOP^101^1^1^^^S|3|||37^REID^TIMOTHY^Q^^^^^^AccMgr^^^^CI|||01||||1|||37^REID^TIMOTHY^Q^^^^^^AccMgr^^^^CI|2|40007716^^^AccMgr^VN|4|||||||||||||||||||1||G|||20050110045253||||||
GT1|1|010107127^^^MS4^PN^|BARRETT^JEAN^S^^|BARRETT^LAWRENCE^E^^|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^|(818)111-3361||19301013|F||A|354-22-1840||||RETIRED|^^^^00000^|||||||20130711|||||0000007496|W||||||||Y|||CHR||||||||RETIRED||||||C
IN1|1|0423|MEDICARE IP|^^^^     |||||||19951001|||MCR|BARRETT^JEAN^S^^|A|19301013|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^^^|||1||||||||||||||354221840A|||||||F|^^^^00000^|N||||010107127
IN2||354221840|0000007496^RETIRED|||354221840A||||||||||||||||||||||||||||||Y|||CHR||||W|||RETIRED|||||||||||||||||(818)249-3361||||||||C
IN1|2|0423|2304|AETNA PPO|PO BOX 14079^PO BOX 14079^LEXINGTON^KY^40512|||081140101400020|RETIRED|||20130101|||COM|BARRETT^JEAN^S^^|A|19301013|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^^^|||2||||||||||||||811001556|||||||F|^^^^00000^|N||||010107127
IN2||354221840|0000007496^RETIRED|||||||||||||||||||||||||||||||||Y|||CHR||||W|||RETIRED|||||||||||||||||(818)249-3361||||||||C
")


(def invalid-hl7 ["MSH|^"  "^~\\&|AccMgr|1|||20151015200643||ADT^A01|599102|P|2.3|foo||
EVN|A01|20151010045502|||||
PID|1|010107111^^^MS4^PN^|1609220^^^MS4^MR^001|1609220^^^MS4^MR^001|BARRETT^JEAN^SANDY^^||19440823|F||C|STRAWBERRY AVE^FOUR OAKS LODGE^ALBUKERKA^CA^98765^USA^^||(111)222-3333||ENG|W|CHR|111155555550^^^MS4001^AN^001|123-22-1111||||OKLAHOMA|||||||N
PV1|1|I|PREOP^101^1^1^^^S|3|||37^REID^TIMOTHY^Q^^^^^^AccMgr^^^^CI|||01||||1|||37^REID^TIMOTHY^Q^^^^^^AccMgr^^^^CI|2|40007716^^^AccMgr^VN|4|||||||||||||||||||1||G|||20050110045253||||||
GT1|1|010107127^^^MS4^PN^|BARRETT^JEAN^S^^|BARRETT^LAWRENCE^E^^|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^|(818)111-3361||19301013|F||A|354-22-1840||||RETIRED|^^^^00000^|||||||20130711|||||0000007496|W||||||||Y|||CHR||||||||RETIRED||||||C
IN1|1|0423|MEDICARE IP|^^^^     |||||||19951001|||MCR|BARRETT^JEAN^S^^|A|19301013|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^^^|||1||||||||||||||354221840A|||||||F|^^^^00000^|N||||010107127
IN2||354221840|0000007496^RETIRED|||354221840A||||||||||||||||||||||||||||||Y|||CHR||||W|||RETIRED|||||||||||||||||(818)249-3361||||||||C
IN1|2|0423|2304|AETNA PPO|PO BOX 14079^PO BOX 14079^LEXINGTON^KY^40512|||081140101400020|RETIRED|||20130101|||COM|BARRETT^JEAN^S^^|A|19301013|2820 SYCAMORE AVE^TWELVE OAKS LODGE^MONTROSE^CA^91214^USA^^^|||2||||||||||||||811001556|||||||F|^^^^00000^|N||||010107127
IN2||354221840|0000007496^RETIRED|||||||||||||||||||||||||||||||||Y|||CHR||||W|||RETIRED|||||||||||||||||(818)249-3361||||||||C"])



(def pid-segment "PID|1|010107111^^^MS4^PN^|1609220^^^MS4^MR^001|1609220^^^MS4^MR^001|BARRETT^JEAN^SANDY^^||19440823|F||C|STRAWBERRY AVE^FOUR OAKS LODGE^ALBUKERKA^CA^98765^USA^^||(111)222-3333||ENG|W|CHR|111155555550^^^MS4001^AN^001|123-22-1111||||OKLAHOMA|||||||N")

(def bulk-message (slurp "seeds/hl7_sample_1.hl7"))

(def lines (str/split bulk-message #"\n"))

(def schema (model/schema))

(def seps {:field \|,
           :component \^,
           :subcomponet \&,
           :repetition \~,
           :escape \\})

(def ctx
  {:schema schema
   :separators seps})

;; (def message-parts (hl7-xform ctx lines))
;; (prn (count message-parts))
(def xf-parsed (comp 
                (hl7-xform ctx)
                (map (fn [x] (str/join "\r" x)))
                (map parse)
                ))

(def list-messages (into [] xf-parsed lines))

(clojure.pprint/pprint list-messages)


(deftest truncated-hl7-test
  (testing "Should return 'Message too short' for invalid HL7"
    (is (= ["Message is too short (MSH truncated)"] (is-valid-hl7? (first invalid-hl7))))))


(deftest valid-hl7-message-test
  (testing "Should return errors if message doesn't start with MSH"
    (is (= ["Message should start with MSH segment"] (is-valid-hl7? (second invalid-hl7))))))


(deftest  assert-seperators
  (testing "Seperators should return a map with keys segment, field, component, subcomponent, repetition and escape"
    (let [seps {:field \|,
                :component \^,
                :subcomponent \&,
                :repetition \~,
                :escape \\}]
      (match (select-keys (separators valid-msg) [:field :component :subcomponent :repetition :escape]) seps))))


(deftest util-methods-test
  (testing "should split strings by seperators"
    (match  ["Foo" "Bar" "Baz^FooBaz"] (split-by "Foo|Bar|Baz^FooBaz" \|))
    (match  ["Foo|Bar|Baz" "FooBaz"] (split-by "Foo|Bar|Baz^FooBaz" \^))))


(deftest create-index-map
  (testing "Create Indexed Map for various types of collections"
    (match {1 43, 2 23, 3 44} (indexed-map '(43 23 44)))
    (match {1 :MSH 2 :PD1 3 :PV1} (indexed-map [:MSH :PD1 :PV1]))
    (match {1 [:foo {:field 1}], 2 [:bar {:component 23}], 3 [:baz {:repetition 23}]}
      (indexed-map {:foo {:field 1} :bar {:component 23} :baz {:repetition 23}}))))


(deftest not-empty-assert
  (testing "Assert if given vector, sequence or string is an usable entity or not"
    (is (false? (not-empty? {})))
    (is (true? (not-empty? {1 2})))
    (is (true? (not-empty? '(1))))
    (is (false? (not-empty? '())))
    (is (false? (not-empty? [])))
    (is (true? (not-empty? [1])))
    (is (false? (not-empty? " ")))
    (is (false? (not-empty?  "")))
    (is (true? (not-empty?  "some thing")))))

(deftest parse-segments-test
  (reset! model/*model nil)
  #_{:clj-kondo/ignore [:inline-def]}
  (def schema (model/schema))

  #_{:clj-kondo/ignore [:inline-def]}
  (def seps {:field \|,
             :component \^,
             :subcomponent \&,
             :repetition \~,
             :escape \\})

  #_{:clj-kondo/ignore [:inline-def]}
  (def ctx
    {:schema schema
     :separators seps})
  (is (= (dissoc (separators valid-msg) :segment) seps))

  (testing "Assert Value"
    (match (parse-value ctx  (ordered-map [:type "IS"] [:name "HD.1"] [:key "ns"] [:desc "Namespace ID"]) "MS4") "MS4"))

  (testing "Parse Messge segments into shape"
    (match
     (parse-segment
      ctx "MSH|^~\\&|AccMgr|1|||20151015200643||ADT^A01|599102|P|2.3|foo||" {})
      ["MSH"
       {:type {:code "ADT", :event "A01"},
        :id "599102",
        :encoding "^~\\&",
        :seqnum "foo",
        :version {:id "2.3"},
        :proc_id {:proc_id "P"},
        :datetime {:time "20151015200643"},
        :facility {:ns "1"},
        :app {:ns "AccMgr"}
        :separator "|"}]))

  (testing "Assert Field Segments"
    (match
     (parse-segment
      ctx  pid-segment {})
      ["PID"
       {:religion {:code "CHR"},
        :patient_id  {:value "010107111",
                      :authority {:ns "MS4"},
                      :system "PN"},
        :alternate_id [{:value "1609220",
                        :authority {:ns "MS4"},
                        :system "MR",
                        :facility {:ns "001"}}],
        :account_number {:value "111155555550",
                         :authority {:ns "MS4001"},
                         :system "AN",
                         :facility {:ns "001"}},
        :race [{:code "C"}],
        :gender "F",
        :birth_date {:time "19440823"},
        :primary_language  {:code "ENG"},
        :home_phone [{:phone "(111)222-3333"}],
        :ssn_number "123-22-1111",
        :birth_place "OKLAHOMA",
        :address [{:street {:text "STRAWBERRY AVE"}
                   :text "FOUR OAKS LODGE",
                   :city "ALBUKERKA",
                   :state "CA",
                   :postal_code "98765",
                   :country "USA"}],
        :set_id "1",
        :name [{:family {:surname "BARRETT"},
                :given "JEAN",
                :initials "SANDY"}],
        :identifiers [{:value "1609220",
                       :authority {:ns "MS4"},
                       :system "MR",
                       :facility {:ns "001"}}],
        :marital_status {:code "W"},
        :death_indicator "N"}])

    (match
     (parse-segment
      ctx "PID|1|312626^^^^^Main Lab&05D0557149&CLIA|0362855^^^^^Main Lab&05D0557149&CLIA|^^^^^Main Lab&05D0557149&CLIA|LOPEZ^ADALBERTO||19450409|M|||8753 APPERSON ST^^SUNLAND^CA^91040||(818)429-5631|||||000016715153|572458313" {})

      ["PID" {:identifiers [{:value "0362855",
                             :facility {:ns "Main Lab", :uid "05D0557149", :type "CLIA"}}]}])))


