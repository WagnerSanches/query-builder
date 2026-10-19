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
      (let [estagios (compilador/compilar-estagios (:estagios c))
            terminal (if-let [t (:terminal c)] (compilador/compilar-terminal t) identity)]
        (terminal (estagios (:registros dados)))))))

(defn formatar-decimal [v]
  (String/format java.util.Locale/US "%.2f" (to-array [(double v)])))

(defn- formatar-valor [tipo v]
  (case tipo
    :decimal (formatar-decimal v)
    :inteiro (str v)
    :texto   v))

(defn formatar-registro [esquema campos registro]
  (let [tipos (into {} esquema)]
    (str/join "; " (map #(formatar-valor (tipos %) (get registro %)) campos))))

(defn- tipo-da-agregacao [tipos agregacao]
  (case (first agregacao)
    :contar :inteiro
    :soma   (tipos (second agregacao))
    :media  :decimal))

(defn- ou-vazio [linhas]
  (if (empty? linhas) ["(vazio)"] linhas))

(defn formatar-registros [esquema campos registros]
  (ou-vazio (map #(formatar-registro esquema campos %) registros)))

(defn formatar-grupos [esquema terminal grupos]
  (let [[_ campo agregacao] terminal
        tipos      (into {} esquema)
        tipo-chave (tipos campo)
        tipo-valor (tipo-da-agregacao tipos agregacao)]
    (ou-vazio
      (map (fn [[chave valor]]
             (str (formatar-valor tipo-chave chave) "; " (formatar-valor tipo-valor valor)))
           grupos))))

(defn- formatar-terminal [esquema terminal valor]
  (formatar-valor (tipo-da-agregacao (into {} esquema) terminal) valor))

(defn consultar [dados texto]
  (let [ast       (analisar texto)
        resultado (executar ast dados)
        esquema   (:esquema dados)
        terminal  (:terminal ast)]
    (cond
      (:erros resultado)                [(str "ERRO: " (str/join ", " (:erros resultado)))]
      (nil? terminal)                   (formatar-registros esquema (compilador/campos-de-saida ast esquema) resultado)
      (= :agrupar-por (first terminal)) (formatar-grupos esquema terminal resultado)
      :else                             [(formatar-terminal esquema terminal resultado)])))