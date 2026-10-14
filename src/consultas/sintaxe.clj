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



(defn consumir [tokens]
  [(first tokens) (rest tokens)])

(defn comparacao [tokens]
  (let [[[_ campo]          resto] (consumir tokens)
        [[tipo-op op]       resto] (consumir resto)
        [[tipo-lit literal] resto] (consumir resto)]
    (cond
      (not= tipo-op :op)
      (throw (ex-info (str "operador desconhecido depois de " campo (when op (str ": " op)))
                      {:campo campo}))

      (not (#{:numero :texto} tipo-lit))
      (throw (ex-info (str "falta o literal depois de " campo " " (name op))
                      {:campo campo}))

      :else
      [[op (keyword campo) literal] resto])))

(declare expressao)

(defn unidade [tokens]
  (cond
    (= (first tokens) [:palavra "nao"])
    (let [[no resto] (unidade (rest tokens))]
      [[:nao no] resto])

    (= (first tokens) [:abre])
    (let [[no resto] (expressao (rest tokens))]
      (if (= (first resto) [:fecha])
        [no (rest resto)]
        (throw (ex-info "parentese sem fechar" {}))))

    :else
    (comparacao tokens)))

(defn nivel-e [tokens]
  (let [[primeiro resto] (unidade tokens)]
    (loop [no primeiro
           resto resto]
      (if (= (first resto) [:palavra "e"])
        (let [[direita resto] (unidade (rest resto))]
          (recur [:e no direita] resto))
        [no resto]))))

(defn expressao [tokens]   ; nível "ou"
  (let [[primeiro resto] (nivel-e tokens)]
    (loop [no primeiro
           resto resto]
      (if (= (first resto) [:palavra "ou"])
        (let [[direita resto] (nivel-e (rest resto))]
          (recur [:ou no direita] resto))
        [no resto]))))

(defn separar-elementos [tokens]
  (if (empty? tokens)
    []
    (loop [ts tokens
           elementos []]
      (let [[elemento resto] (split-with #(not= % [:pipe]) ts)]
        (when (empty? elemento)
          (throw (ex-info "estagio vazio" {})))
        (if (empty? resto)
          (conj elementos elemento)
          (recur (rest resto) (conj elementos elemento)))))))

(defn estagio-onde [tokens]          ; tokens começa depois de "onde"
  (let [[expr resto] (expressao tokens)]
    [[:onde expr] resto]))

(defn estagio-ordenar [tokens]       ; tokens começa depois de "ordenar"
  (let [[_por resto]      (consumir tokens)
        [[_ campo] resto] (consumir resto)
        desc?             (= (first resto) [:palavra "desc"])
        resto             (if desc? (rest resto) resto)]
    [[:ordenar-por (keyword campo) (if desc? :desc :asc)] resto]))

(defn estagio-limitar [tokens]       ; tokens começa depois de "limitar"
  (let [[[_ n] resto] (consumir tokens)]
    [[:limitar n] resto]))

(defn estagio-selecionar [tokens]    ; tokens começa depois de "selecionar"
  (loop [ts tokens
         campos []]
    (let [[[_ campo] resto] (consumir ts)
          campos (conj campos (keyword campo))]
      (if (= (first resto) [:virgula])
        (recur (rest resto) campos)
        [[:selecionar campos] resto]))))

(defn estagio [elemento]
  (let [[primeiro resto] (consumir elemento)]
    (cond
      (= primeiro [:palavra "onde"])       (estagio-onde resto)
      (= primeiro [:palavra "ordenar"])    (estagio-ordenar resto)
      (= primeiro [:palavra "limitar"])    (estagio-limitar resto)
      (= primeiro [:palavra "selecionar"]) (estagio-selecionar resto))))