(ns consultas.compilador)

(defn- erros-no [esquema no]
  (case (first no)
    :onde        []
    :ordenar-por []
    :limitar     []
    :selecionar  []
    :contar      []
    :soma        []
    :media       []
    :agrupar-por []))

(defn validar [ast esquema]
  (let [nos (cond-> (:estagios ast)
              (:terminal ast) (conj (:terminal ast)))]
    (vec (mapcat #(erros-no esquema %) nos))))