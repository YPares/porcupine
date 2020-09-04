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

## Porcupine's development

Porcupine's development happens mainly inside [NovaDiscovery](https://www.novadiscovery.com)'s internal codebase, where a porcupine's fork resides.
But we often synchronise this internal repo and [porcupine's github repo](https://github.com/tweag/porcupine).
This is why commits tend to appear by batches on porcupine's github.

Lately, a lot of effort has been invested in developping [Kernmantle](https://github.com/tweag/kernmantle) which should provide the new task representation (see below in Future plans).

### Participating to porcupine's development

Issues and MRs are welcome :)

### Future plans

These features are being developed and should land soon:

- `porcupine-servant`: a servant app can directly serve porcupine's pipelines as routes, and expose a single configuration for the whole server
- enhancement of the API to run tasks: `runPipelineTask` would remain in place but be a tiny wrapper over a slightly lower-level API. This makes it easier to run pipelines in different contexts (like that of `porcupine-servant`)
- common configuration representation: for now porcupine can only handle config via a yaml/json file + CLI. Some applications can require other configuration sources (GraphQL, several config files that override one another, etc). We want to have a common tree format that every configuration source get translated too, and just merge all these trees afterwards, so each config source is fully decoupled from the others and can be activated at will

The following are things we'd like to start working on:

- switch to [`cas-store`](http://hackage.haskell.org/package/cas-store-1.0.1): porcupine's dependency on `funflow` is mainly for the purpose of caching. Now that `cas-store` is a separate project, porcupine can directly depend on it. This will simplify the implementation of `PTask` and make it easier to integrate `PTask`s with other libraries.
- implement `PTask` over a [Kernmantle Rope](https://github.com/tweag/kernmantle): this is the main reason we started the work on Kernmantle, so it could become a uniform pipeline API, independent of the effects the pipeline performs (caching, collecting options or required resources, etc). Both porcupine and [funflow](https://github.com/tweag/funflow) would become collections of Kernmantle effects and handlers, and would therefore be seamlessly interoperable. Developpers would also be able to add their own custom effects to a pipeline. This would probably mean the death of `reader-soup`, as the LocationAccessors could directly be embedded as Kernmatle effects.
- package porcupine's `VirtualTree` as a separate package: all the code that is not strictly speaking related to tasks would be usable separately (for instance to be used in Kernmantle effects handlers).

## F.A.Q.

#### How are Porcupine and [Funflow](https://github.com/tweag/funflow) related?

Porcupine uses Funflow internally to provide caching. Funflow's API is centered around the
ArrowFlow class. PTask (porcupine's main computation unit) implements ArrowFlow
too, so usual funflow operations are usable on PTasks too.

Aside from that, funflow and porcupine don't operate at the same level of
abstraction: funflow is for software devs building applications the way they
want, while porcupine is higher-level and more featureful, and
targets software devs at the same time as modelers or data analysts. However, porcupine
doesn't make any choice in terms of computation, visualization, etc. libraries or
anything. That part is still up to the user.

The main goal of Porcupine is to be a tool to
structure your app, a backbone that helps you kickstart e.g. a data
pipeline/analytics application while keeping the boilerplate (config, I/O) to a
minimum, while providing a common framework if you have code (tasks, serializing
functions) to share between several applications of that type. 
But since the arrow and caching API is the same in both Funflow and Porcupine, as a software dev you can start by
using porcupine, and if you realize you don't actually need the high level
features (config, rebinding of inputs, logging, etc) then drop the dependency
and transition to Funflow's level.

#### Can the tasks run in a distributed fashion?

Funflow provides a worker demon that the main pipeline can distribute docker-containerized tasks to. For pure Haskell functions, there is [funflow-jobs](https://github.com/tweag/funflow/tree/master/funflow-jobs) but it's experimental.

So it could be used with funflow-jobs, but for now porcupine has only ever been used for parallel execution of tasks.
We recently started thinking about how the funflow/porcupine's model could be adapted to run a pipeline in a cluster in a decentralized fashion, and we have some promising ideas so that feature may appear in the future.

Another solution (which is the one used by our client) is to use an external job queue (like celery) which starts porcupine pipeline instances. This is made easy by the fact that all the configuration of a pipeline instance is exposed by porcupine, and therefore can be set by the program that puts the jobs in the queue (as one JSON file).

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
