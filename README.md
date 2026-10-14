# Colaboradores

- Wagner Aguiar Sanches Garcia Sobrinho

# Sobre o projeto

Motor de consultas em Clojure que carrega registros de um arquivo CSV e executa consultas escritas em texto, como filtrar, ordenar, projetar, agrupar e agregar. A consulta é transformada numa AST feita só de dados e depois compilada numa composição de funções puras, sem estado mutável.

## Forma da AST

Uma consulta é um mapa com duas chaves:

- `:estagios`: vetor de nós que transformam uma sequência de registros em outra, na ordem em que aparecem na consulta.
- `:terminal`: um nó que transforma a sequência num único valor, ou `nil` quando não há terminal.

Todo nó é um vetor cujo primeiro elemento é uma palavra-chave com o tipo do nó. Campos são sempre palavras-chave.

A consulta vazia (`QUERY` sem nada) é `{:estagios [] :terminal nil}`.

### Estágios (em `:estagios`)

| Nó | Texto |
|---|---|
| `[:onde expr]` | `onde <expr>` |
| `[:ordenar-por campo ordem]` | `ordenar por <campo> [desc]`; `ordem` é `:asc` ou `:desc`, sempre explícita |
| `[:limitar n]` | `limitar <n>` |
| `[:selecionar [campo ...]]` | `selecionar <campo>, <campo>, ...` |

### Terminais (em `:terminal`)

| Nó | Texto |
|---|---|
| `[:contar]` | `contar` |
| `[:soma campo]` | `soma <campo>` |
| `[:media campo]` | `media <campo>` |
| `[:agrupar-por campo agregacao]` | `agrupar por <campo> com <agregacao>`; `agregacao` é um dos três nós acima |

### Expressões (dentro de `:onde`)

| Nó | Texto |
|---|---|
| `[op campo literal]` | `<campo> <op> <literal>`; `op` é `:=`, `:!=`, `:<`, `:<=`, `:>` ou `:>=` |
| `[:e a b]` | `a e b` |
| `[:ou a b]` | `a ou b` |
| `[:nao a]` | `nao a` |

O literal mantém o tipo em que foi escrito: inteiro (`8`), decimal (`8.6`) ou texto (`"drama"`). Correntes do mesmo operador associam à esquerda: `a e b e c` vira `[:e [:e a b] c]`.

### Exemplos

**1. Filtro, ordenação, limite e projeção**

```
QUERY onde ano > 2010 | ordenar por ano | limitar 2 | selecionar titulo, ano
```

```clojure
{:estagios [[:onde [:> :ano 2010]]
            [:ordenar-por :ano :asc]
            [:limitar 2]
            [:selecionar [:titulo :ano]]]
 :terminal nil}
```

Saída: `Que Horas Ela Volta; 2015` e `Aquarius; 2016`.

**2. Expressão com `ou`, `e`, `nao` e parênteses**

```
QUERY onde (genero = "acao" ou genero = "suspense") e nao ano < 2010 | selecionar titulo
```

```clojure
{:estagios [[:onde [:e [:ou [:= :genero "acao"] [:= :genero "suspense"]]
                       [:nao [:< :ano 2010]]]]
            [:selecionar [:titulo]]]
 :terminal nil}
```

Saída: `Bacurau`.

**3. Filtro com agrupamento**

```
QUERY onde ano >= 2010 | agrupar por genero com contar
```

```clojure
{:estagios [[:onde [:>= :ano 2010]]]
 :terminal [:agrupar-por :genero [:contar]]}
```

Saída: `drama; 3` e `suspense; 1`.