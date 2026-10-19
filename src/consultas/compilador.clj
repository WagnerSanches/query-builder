(ns consultas.compilador)

(defn- erro-campo [esquema campo]
  (if ((into {} esquema) campo)
    []
    [(str "campo inexistente: " (name campo))]))

(defn- erros-campos [esquema campos]
  (vec (mapcat #(erro-campo esquema %) (distinct campos))))

(defn- campos-da-expr [expr]
  (case (first expr)
    (:e :ou)               (concat (campos-da-expr (nth expr 1)) (campos-da-expr (nth expr 2)))
    :nao                   (campos-da-expr (second expr))
    (:= :!= :< :<= :> :>=) [(second expr)]))

(defn- tipo-do-literal [v]
  (cond
    (string? v)  :texto
    (integer? v) :inteiro
    :else        :decimal))

(defn- numerico? [tipo]
  (contains? #{:inteiro :decimal} tipo))

(defn- erros-tipo-expr [esquema expr]
  (case (first expr)
    (:e :ou)               (concat (erros-tipo-expr esquema (nth expr 1))
                                   (erros-tipo-expr esquema (nth expr 2)))
    :nao                   (erros-tipo-expr esquema (second expr))
    (:= :!= :< :<= :> :>=) (let [[_ campo literal] expr
                                 tipo-campo ((into {} esquema) campo)
                                 tipo-lit   (tipo-do-literal literal)]
                             (if (or (nil? tipo-campo)
                                     (= (numerico? tipo-campo) (numerico? tipo-lit)))
                               []
                               [(str "comparacao entre " (name tipo-campo) " e " (name tipo-lit)
                                     " no campo " (name campo))]))))
(defn- erro-numerico [esquema agregacao campo]
  (let [tipo ((into {} esquema) campo)]
    (if (and tipo (not (numerico? tipo)))
      [(str (name agregacao) " exige campo numerico: " (name campo))]
      [])))

(defn- erros-ordem [nos]
  (if (some #(= :selecionar (first %)) (butlast nos))
    ["selecionar tem que ser o ultimo elemento da consulta"]
    []))
    
(defn- erros-no [esquema no]
  (case (first no)
    :ordenar-por (erros-campos esquema [(second no)])
    :limitar     []
    :selecionar  (erros-campos esquema (second no))
    :contar      []
    :soma        (into (erros-campos esquema [(second no)])
                       (erro-numerico esquema :soma (second no)))
    :media       (into (erros-campos esquema [(second no)])
                       (erro-numerico esquema :media (second no)))
    :agrupar-por (let [[_ campo agregacao] no]
                   (into (erros-campos esquema [campo])
                         (erros-no esquema agregacao)))
    :onde        (into (erros-campos esquema (campos-da-expr (second no)))
                       (erros-tipo-expr esquema (second no)))))



(defn validar [ast esquema]
  (let [nos (cond-> (:estagios ast)
              (:terminal ast) (conj (:terminal ast)))]
    (into (vec (mapcat #(erros-no esquema %) nos))
          (erros-ordem nos))))


(def ^:private testes
  {:=  zero?
   :!= #(not (zero? %))
   :<  neg?
   :<= #(<= % 0)
   :>  pos?
   :>= #(>= % 0)})

(defn- compilar-comparacao [[op campo literal]]
  (let [teste (testes op)]
    (fn [registro]
      (teste (compare (get registro campo) literal)))))

(defn compilar-expr [expr]
  (case (first expr)
    :e   (let [a (compilar-expr (nth expr 1))
               b (compilar-expr (nth expr 2))]
           (fn [registro] (and (a registro) (b registro))))
    :ou  (let [a (compilar-expr (nth expr 1))
               b (compilar-expr (nth expr 2))]
           (fn [registro] (or (a registro) (b registro))))
    :nao (let [a (compilar-expr (second expr))]
           (fn [registro] (not (a registro))))
    (:= :!= :< :<= :> :>=) (compilar-comparacao expr)))   

(defn compilar-estagio [no]
  (case (first no)
    :onde        (partial filter (compilar-expr (second no)))
    :limitar     (partial take (second no))
    :selecionar  (let [campos (second no)]
                   (partial map #(select-keys % campos)))
    :ordenar-por (let [[_ campo ordem] no
                       comparador (case ordem
                                    :asc  compare
                                    :desc #(compare %2 %1))]
                   (partial sort-by campo comparador))))

(defn campos-de-saida [ast esquema]
  (if-let [sel (some #(when (= :selecionar (first %)) %) (:estagios ast))]
    (second sel)
    (mapv first esquema)))

(defn compilar-estagios [estagios]
  (reduce (fn [composta estagio] (comp estagio composta))
          identity
          (map compilar-estagio estagios)))
(defn- contar [registros]
  (reduce (fn [n _] (inc n)) 0 registros))

(defn- somar [campo registros]
  (reduce (fn [total r] (+ total (get r campo))) 0 registros))

(defn- media [campo registros]
  (let [[total n] (reduce (fn [[total n] r] [(+ total (get r campo)) (inc n)])
                          [0 0]
                          registros)]
    (if (zero? n) 0.0 (/ (double total) n))))

(defn- agrupar [campo agregar registros]
  (->> (group-by campo registros)
       (sort-by key)
       (map (fn [[chave grupo]] [chave (agregar grupo)]))))

(defn compilar-terminal [no]
  (case (first no)
    :contar      contar
    :soma        (partial somar (second no))
    :media       (partial media (second no))
    :agrupar-por (let [[_ campo agregacao] no]
                   (partial agrupar campo (compilar-terminal agregacao)))))