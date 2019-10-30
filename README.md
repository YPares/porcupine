![logo](porcupine.svg)

[![CircleCI](https://circleci.com/gh/tweag/porcupine/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/porcupine/tree/master)
[![Join the chat at
https://gitter.im/tweag/porcupine](https://badges.gitter.im/tweag/porcupine.svg)](https://gitter.im/tweag/porcupine?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


Porcupine is a tool aimed at people who want to express in Haskell general data manipulation and analysis tasks,

1. In a way that is agnostic from the source of the input data and from the
destination of the end results,
2. So that a pipeline can be re-executed in a different environment and on
different data without recompiling, by just a shift in its configuration,
3. While facilitating code reusability (any task can always be reused as part
of a bigger pipeline).

Porcupine specifically targets teams containing skills ranging from those of
data scientists to those of data/software engineers.

## Resources

- [Porcupine GitHub pages](https://tweag.github.io/porcupine/), with an overview and tutorials
- [Introduction to porcupine @Haskell Exchange, in London, October 11th, 2019](https://skillsmatter.com/skillscasts/14236-porcupine-flows-your-rows-with-arrows)
- [Porcupine announcement blog post](https://www.tweag.io/posts/2019-10-30-porcupine.html)

## F.A.Q.

#### How are Porcupine and [Funflow](https://github.com/tweag/funflow) related?

Porcupine uses Funflow internally. Funflow's API is centered around the
ArrowFlow class. PTask (porcupine's main computation unit) implements ArrowFlow
too, so usual funflow operations are usable on PTasks too.

Aside from that, funflow and porcupine don't operate at the same level of
abstraction: funflow is for software devs building applications the way they
want, while porcupine is higher-level and more opinionated and featureful, and
targets software devs at the same time as data analysts. However, porcupine
doesn't make any choice in terms of computation/visualization library or
anything. That part is still up to the user.

But since the caching/arrow API is the same, as a software dev you can start by
using porcupine, and if you realize you don't actually need the high level
features (config, rebinding of inputs, logging, etc) then drop the dependency
and transition to funflow's level. The main goal of porcupine is to be a tool to
structure your app, a backbone that helps you kickstart e.g. a data
pipeline/analytics application while keeping the boilerplate (config, I/O) to a
minimum, while providing a common framework if you have code (tasks, serializing
functions) to share between several applications of that type.

#### I like the idea of tasks that automatically maintain and merge their requirements when they compose, but I want to deal with configuration, CLI and everything myself. Can I do that?

Of course! That means you would replace the call to `runPipelineTask` by custom
code. You want to have a look at the `splitTask` lens. It will separate a task
in its two components: its `VirtualTree` of requirements (which you can treat
however you please, the goal being to turn it into a `DataAccessTree`) and a
`RunnableTask` which you can feed to `execRunnableTask` once you have composed a
`DataAccessTree` to feed it. Although note that this part of the API might
change a bit in future versions.

#### Is Porcupine related to [Hedgehog](http://hackage.haskell.org/package/hedgehog)?

Can see where that comes from ^^, but nope, not all
[R.O.U.S.s](http://imoviequotes.com/wp-content/uploads/2014/11/10-02-The-Princess-Bride-quotes.jpg)
are related. (And also, hedgehogs aren't rodents)

Although we do have a few tests using Hedgehog (and will possibly add more).
