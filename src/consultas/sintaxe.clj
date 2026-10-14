(ns consultas.sintaxe
  (:require [clojure.string :as str]))

(defn- letra?  [c] (or (Character/isLetter c) (= c \_)))
(defn- digito? [c] (Character/isDigit c))

(defn- ler-enquanto [pred cs]
  [(apply str (take-while pred cs)) (drop-while pred cs)])

(defn- ler-palavra [cs]
  (let [[palavra resto] (ler-enquanto #(or (letra? %) (digito? %)) cs)]
    [[:palavra palavra] resto]))

(defn- ler-numero [cs]
  (let [[s resto] (ler-enquanto #(or (digito? %) (= % \.)) cs)]
    [[:numero (if (str/includes? s ".") (parse-double s) (parse-long s))] resto]))

(defn- ler-texto [cs]  
  (let [[conteudo resto] (ler-enquanto #(not= % \") cs)]
    [[:texto conteudo] (rest resto)])) 

(def ^:private operadores-duplos  {"<=" :<= ">=" :>= "!=" :!=})
(def ^:private operadores-simples {\= :=  \< :<  \> :>})

(defn- espaco? [c] (Character/isWhitespace c))
(def ^:private simbolos {\| [:pipe] \, [:virgula] \( [:abre] \) [:fecha]})


(defn- ler-texto [cs]   
  (let [[conteudo resto] (ler-enquanto #(not= % \") cs)]
    (if (empty? resto)
      (throw (ex-info "aspas sem fechar" {:texto conteudo}))
      [[:texto conteudo] (rest resto)])))

(defn tokenizar [texto]
  (loop [cs (seq texto)
         tokens []]
    (if (empty? cs)
      tokens
      (let [c (first cs)
            dois (apply str (take 2 cs))]
        (cond
          (espaco? c)              (recur (rest cs) tokens)
          (operadores-duplos dois) (recur (drop 2 cs) (conj tokens [:op (operadores-duplos dois)]))
          (operadores-simples c)   (recur (rest cs) (conj tokens [:op (operadores-simples c)]))
          (simbolos c)             (recur (rest cs) (conj tokens (simbolos c)))
          (letra? c)  (let [[tok resto] (ler-palavra cs)]
                        (recur resto (conj tokens tok)))
          (digito? c) (let [[tok resto] (ler-numero cs)]
                        (recur resto (conj tokens tok)))
          (= c \")    (let [[tok resto] (ler-texto (rest cs))]
                        (recur resto (conj tokens tok)))
          :else (throw (ex-info (str "caractere desconhecido: " c) {:caractere c})))))))