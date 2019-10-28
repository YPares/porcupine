![logo](porcupine.svg)

[![CircleCI](https://circleci.com/gh/tweag/porcupine/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/porcupine/tree/master)
[![Join the chat at
https://gitter.im/tweag/porcupine](https://badges.gitter.im/tweag/porcupine.svg)](https://gitter.im/tweag/porcupine?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


`porcupine` is a tool aimed at people who want to express in Haskell general data manipulation and analysis tasks,

1. in a way that is agnostic from the source of the input data and from the
destination of the end results,
2. such that a pipeline can be re-executed in a different environment and on
different data without recompiling, by just a shift in its configuration,
3. while facilitating code reusability (any task can always be reused as part
of a bigger pipeline).

`porcupine` specifically targets teams containing skills ranging from those of data scientists
to those of data/software engineers.

# Resources

- [Porcupine's github pages](https://tweag.github.io/porcupine/)
- [Introduction to porcupine @Haskell Exchange, in London, October 11th, 2019](https://skillsmatter.com/skillscasts/14236-porcupine-flows-your-rows-with-arrows)
