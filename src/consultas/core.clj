(ns consultas.core
  (:require [clojure.string :as str]
            [consultas.sintaxe :as sintaxe]
            [consultas.compilador :as compilador]))

(defn linhas [texto]
  (->> (str/split-lines texto)
       (remove str/blank?)))

(defn campos [linha]
  (str/split linha #";"))

(defn coluna [texto]
  (let [[nome tipo] (str/split texto #":")]
    [(keyword nome) (keyword tipo)]))

(defn esquema [cabecalho]
  (mapv coluna (campos cabecalho)))

(def conversores
  {:inteiro parse-long
   :decimal parse-double
   :texto   identity})

(defn converter [tipo texto]
  ((conversores tipo) texto))

(defn valores [esquema linha]
  (mapv converter (map second esquema) (campos linha)))

(defn registro [esquema linha]
  (zipmap (map first esquema) (valores esquema linha)))

(defn carregar [texto]
  (let [[cabecalho & resto] (linhas texto)
        esq (esquema cabecalho)]
    {:esquema   esq
     :registros (mapv #(registro esq %) resto)}))

(defn analisar [texto]
  (sintaxe/analisar texto))

(defn consulta []
  {:estagios [] :terminal nil})

(defn onde [c expr]
  (update c :estagios conj [:onde expr]))

(defn ordenar-por [c campo ordem]
  (update c :estagios conj [:ordenar-por campo ordem]))

(defn limitar [c n]
  (update c :estagios conj [:limitar n]))

(defn selecionar [c campos]
  (update c :estagios conj [:selecionar campos]))

(defn executar [c dados]
  (let [erros (compilador/validar c (:esquema dados))]
    (if (seq erros)
      {:erros erros}
      (:registros dados))))