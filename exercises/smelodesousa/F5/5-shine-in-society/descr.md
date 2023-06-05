<script>
MathJax = {
  loader: {load: ['input/asciimath', 'output/chtml']},
  asciimath: {
    delimiters: [['$','$'], ['`','`']]
  }
}
</script>

<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
<script type="text/javascript" id="MathJax-script" async
  src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/startup.js"></script>

# Introduction

After all, shining at a dinner party is easy, being the star of a _vernissage_ is as easy as eating a _canapé_. Simply apply this foolproof recipe. Randomly select one element from each column listed here and join them in the order of the sets to form a sentence... _et voilá_.

For example, **"The diagnosis identifies the institutional blocks of common practice"**. 

 
```ocaml
|         Set 1         |     Set 2     |          Set 3           |         Set 4         |          Set 5          |
| :-------------------: | :-----------: | :----------------------: |  :------------------: |  :-------------------:  |
|   "The excellence"    |  "reinforces" |    "the institutional"   |       "factors"       |     "of performance"    |
|   "The intervention"  |  "mobilizes"  |   "the organizational"   |      "processes"      |     "of the device"     |
|     "The goal"        |   "reveals"   |     "the qualitative"    |     "parameters"      |     "of the company"    |
|   "The diagnosis"     |  "stimulates" |     "the analytical"     |     "progresses"      |      "of the group"     |
| "The experimentation" |  "modifies"   |   "the characteristic"   |      "concepts"       |  "of the beneficiaries" |
|    "The formation"    |  "clarifies"  |    "the motivational"    | "different know-hows" |     "of the hierarchy"  |
|   "The evaluation"    |   "renews"    |    "the pedagogical"     |      "problems"       |   "of common practice"  |
|   "The purpose"       | "identifies"  |    "the representative"  |     "indicators"      |   "of the procedures"   |
|    "The expression"   |  "perfects"   |    "the contributory"    |       "results"       |      "of the actors"    |
|   "The management"    |  "develops"   |    "the cumulative";     |      "effects";       |   "of the problems"  |
|    "The method"       |  "dynamizes"  |      "the strategic"     |       "blocks"        |    "of the structures"  |
|   "The experience"    |  "programs"   |  "the neuro-linguistic"  |    "prerequisites"    |   "of the meta-context" |
|   "The reframing"     |   "scores"    |      "the systemic"      |      "paradoxes"      |   "of the organization" |
```

Assume that each set is organized in the form of an already declared vector, in the prelude, with the corresponding names: `v1`, `v2`, `v3`, `v4` and `v5`.

# Goal

The purpose of this exercise is to implement a function `speak_vacantly : unit -> string` that randomly produces a sentence, using the contents of the previously mentioned vectors. For example: `speak_vacantly () -> "The excellence perfects the cumulative different know-hows of the device"`.


For this purpose, you can use the function `Random.int` from the OCaml module `Random` (see https://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html) 


> Random.int : int -> int
>
> Random.int bound returns a random integer between $0$ (inclusive) 
>  and `bound` (exclusive). `bound` must be greater than $0$ and less than $2^30$.


Using this function, you can obtain integers belonging to the range $\{0...12\}$. Please note that *it is not expected* that you use functions such as `Random.init` or `Random.self_init`.