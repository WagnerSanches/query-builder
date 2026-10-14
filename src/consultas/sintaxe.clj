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

(defn- exigir [tokens esperado mensagem]
  (let [[token resto] (consumir tokens)]
    (if (= token esperado)
      [token resto]
      (throw (ex-info mensagem {})))))

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

(defn estagio-ordenar [tokens]
  (let [[_por resto]      (exigir tokens [:palavra "por"] "falta \"por\" depois de ordenar")
        [[_ campo] resto] (consumir resto)
        desc?             (= (first resto) [:palavra "desc"])
        resto             (if desc? (rest resto) resto)]
    [[:ordenar-por (keyword campo) (if desc? :desc :asc)] resto]))

(defn estagio-limitar [tokens]
  (let [[[tipo n] resto] (consumir tokens)]
    (if (and (= tipo :numero) (integer? n))
      [[:limitar n] resto]
      (throw (ex-info "limitar espera um numero inteiro" {})))))

(defn estagio-selecionar [tokens]    ; tokens começa depois de "selecionar"
  (loop [ts tokens
         campos []]
    (let [[[_ campo] resto] (consumir ts)
          campos (conj campos (keyword campo))]
      (if (= (first resto) [:virgula])
        (recur (rest resto) campos)
        [[:selecionar campos] resto]))))

(defn terminal-soma [tokens]    ; tokens começa depois de "soma"
  (let [[[_ campo] resto] (consumir tokens)]
    [[:soma (keyword campo)] resto]))

(defn terminal-media [tokens]   ; tokens começa depois de "media"
  (let [[[_ campo] resto] (consumir tokens)]
    [[:media (keyword campo)] resto]))

(defn agregacao [tokens]
  (let [[primeiro resto] (consumir tokens)]
    (cond
      (= primeiro [:palavra "contar"]) [[:contar] resto]
      (= primeiro [:palavra "soma"])   (terminal-soma resto)
      (= primeiro [:palavra "media"])  (terminal-media resto))))

(defn terminal-agrupar [tokens]
  (let [[_por resto]      (exigir tokens [:palavra "por"] "falta \"por\" depois de agrupar")
        [[_ campo] resto] (consumir resto)
        [_com resto]      (exigir resto [:palavra "com"] "falta \"com <agregacao>\" depois do campo")
        [agr resto]       (agregacao resto)]
    [[:agrupar-por (keyword campo) agr] resto]))

(defn estagio [elemento]
  (let [[primeiro resto] (consumir elemento)]
    (cond
      (= primeiro [:palavra "onde"])       (estagio-onde resto)
      (= primeiro [:palavra "ordenar"])    (estagio-ordenar resto)
      (= primeiro [:palavra "limitar"])    (estagio-limitar resto)
      (= primeiro [:palavra "selecionar"]) (estagio-selecionar resto)
      (= primeiro [:palavra "contar"])     [[:contar] resto]
      (= primeiro [:palavra "soma"])       (terminal-soma resto)
      (= primeiro [:palavra "agrupar"])    (terminal-agrupar resto)
      (= primeiro [:palavra "media"])      (terminal-media resto)
      :else (throw (ex-info (str "estagio desconhecido: " (second primeiro)) {})))))

(def ^:private finais #{:selecionar :contar :soma :media :agrupar-por})

(defn conferir-final [nos]
  (when-let [no (some #(when (finais (first %)) %) (butlast nos))]
    (throw (ex-info (str (name (first no)) " tem que ser o ultimo elemento da consulta") {})))
  nos)

(def ^:private terminais #{:contar :soma :media :agrupar-por})

(defn analisar [texto]
  (let [nos (->> (tokenizar texto)
                 separar-elementos
                 (mapv (comp first estagio))
                 conferir-final)
        ultimo (peek nos)]
    (if (and ultimo (terminais (first ultimo)))
      {:estagios (pop nos) :terminal ultimo}
      {:estagios nos :terminal nil})))