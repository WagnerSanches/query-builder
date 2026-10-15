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

(defn- erro-numerico [esquema agregacao campo]
  (let [tipo ((into {} esquema) campo)]
    (if (and tipo (not (numerico? tipo)))
      [(str (name agregacao) " exige campo numerico: " (name campo))]
      [])))