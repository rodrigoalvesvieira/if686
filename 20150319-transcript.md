# PLC - Primeiro Exercício

### 1. O que é um paradigma de linguagem de programação?

Um paradigma de linguagem de programação é um conjunto de maneiras de se estruturar um programa de computador. Pode ser visto como um estilo de passar o raciocínio do programador para o código.

Enquanto é verdade que a maioria das aplicações pode ser resolvida usando diferentes paradigmas. É missão do programador decidir qual deles é o mais apropriado para sua aplicação.

Diferentes linguagens adotam diferentes paradigmas, sendo algumas delas multi-paradigmas (Python, Ruby, Scala) e outras não (Haskell, SmallTalk).

### 2. Quais são as características fundamentais do paradigma de programação funcional? Como ele se diferencia da programação imperativa?

Na programação funcional, as funções são o foco do código, ao invés de objetos e variáveis. PF não usa variáveis e métodos para manter e alterar estados. Esse paradigma trata de funções no modo mais matemático: uma função recebe valores e retorna **novos** valores. Assim, temos que a **Imutabilidade** é um dos pilares desse conceito.

Funções podem:

1. Ser passadas como parâmetros para outras funções;
2. Ser retornadas por outras funções;
3. Elementos de estruturas de dados

### 3. Quais são as vantagens do paradigma funcional, em contrapartida com o paradigma imperativo?

* Imutabilidade (e com isso, previsibilidade);
* Não há efeitos colaterais;

Por essas características, frequentemente vemos linguagens funcionais associadas a soluções concorrentes e paralelas, nas quais um dos grandes problemas são os **race-conditions** de IO.

### 4. Quais são as origens da programação funcional?

John McCarthy desenvolveu LISP no fim dos anos 50, no MIT.

Ressalva: LISP não é puramente funcional.

### 5. Que grandes empresas utilizam esse paradigma? Que tipo de sistema é desenvolvido usando linguagens funcionais?

Google, Facebook, In Mobi, Netflix