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

(defn- erros-no [esquema no]
  (case (first no)
    :onde        (erros-campos esquema (campos-da-expr (second no)))
    :ordenar-por (erros-campos esquema [(second no)])
    :limitar     []
    :selecionar  (erros-campos esquema (second no))
    :contar      []
    :soma        (erros-campos esquema [(second no)])
    :media       (erros-campos esquema [(second no)])
    :agrupar-por (let [[_ campo agregacao] no]
                   (into (erros-campos esquema [campo])
                         (erros-no esquema agregacao)))))

(defn validar [ast esquema]
  (let [nos (cond-> (:estagios ast)
              (:terminal ast) (conj (:terminal ast)))]
    (vec (mapcat #(erros-no esquema %) nos))))