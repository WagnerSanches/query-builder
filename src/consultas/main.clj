(ns consultas.main
  (:require [clojure.string :as str]
            [consultas.core :as c]))

(defn- responder [dados linha]
  (cond
    (str/blank? linha)                ["ERRO: linha em branco"]
    (= linha "QUERY")                 (c/consultar dados "")
    (str/starts-with? linha "QUERY ") (c/consultar dados (subs linha 6))
    :else                             [(str "ERRO: comando desconhecido: " linha)]))

(defn- linhas-da-resposta [dados linha]
  (try
    (vec (responder dados linha))
    (catch StackOverflowError _
      ["ERRO: consulta aninhada ou longa demais"])
    (catch Exception e
      [(str "ERRO: " (or (ex-message e) (.getSimpleName (class e))))])))

(defn -main [& args]
  (let [dados (c/carregar (slurp (first args)))]
    (loop []
      (print "> ")
      (flush)
      (let [linha (read-line)]
        (when-not (or (nil? linha) (= linha "EXIT"))
          (doseq [l (linhas-da-resposta dados linha)]
            (println l))
          (recur))))))