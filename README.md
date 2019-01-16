# Porcupine

[![CircleCI](https://circleci.com/gh/tweag/porcupine/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/porcupine/tree/master)

Porcupine stands for _Portable, Reusable & Customizable Pipeline_. It is a tool
aimed at data scientists and numerical analysts, so that they can express
general data manipulation and analysis tasks,

1. in a way that is agnostic from the source of the input data and
from the destination of the end results,
2. such that a pipeline can be re-executed in a different environment and on
different data without recompiling, by just a shift in its configuration,
3. while maintaining composability (any task can always be reused as
a subtask of a greater task pipeline).

Porcupine provides three core abstractions: _serials_, _tasks_ and
_resource trees_.

## Serials

A `SerialsFor a b` encompasses functions to write data of type `a` and read data
of type `b`. Porcupine provides a few serials if your datatype already
implements standard serialization interfaces, such as `aeson`'s `To/FromJSON` or
`binary`, and makes it easy to reuse custom serialization functions you might
already have. A `SerialsFor A B` is a collection of `A -> i` and `i -> B`
functions, where `i` can be any intermediary type, most often `ByteString`,
`Data.Aeson.Value` or `Text`.

`SerialsFor` is a [profunctor][profunctor]. That means that once you know how to
(de)serialize an `A` (ie. if you have a `SerialsFor A A`), then you can just use
`dimap` to get a `SerialsFor B B` if you know how to convert `A` to & from
`B`. Handling only one-way serialization or deserialization is perfectly
possible, that just mean you will have `SerialsFor Void B` or `SerialsFor A ()`
and use only `lmap` or `rmap`. Any `SerialsFor a b` is also a semigroup, where
`(<>)` merges together the collections of serialization functions they contain,
meaning that you can for instance gather default serials, or serials from an
external source and add to them your custom serialization methods, before using
it in a task pipeline.

[profunctor]: https://www.stackage.org/haddock/lts-12.21/lens-4.16.1/Control-Lens-Combinators.html#t:Profunctor

The end goal of `SerialsFor` is that the user writing a task pipeline will not
have to care about how the input data will be serialized. As long as the data it
tries to feed into the pipeline matches some known serialization function. Also,
the introspectable nature of resource trees (more on that later) allows you to
_add_ serials to an existing pipeline before reusing it as part of your own
pipeline. This sort of makes Porcupine an "anti-ETL": rather than marshall and
curate input data so that it matches the pipeline expectations, you augment the
pipeline so that it can deal with more data sources.

## Resource tree

Every task in Porcupine exposes a resource tree. Resources are represented in
porcupine by `VirtualFile`s, and a resource tree is a hierarchy (like a
filesystem) of `VirtualFiles`. A `VirtualFile a b` just groups together a
logical path and a `SerialsFor a b`, so it just something with an identifier
(like `"/Inputs/Config"` or `"/Ouputs/Results"`) in which we can write a `A`
and/or from which we can read a `B`. We say the path is "logical" because it
doesn't necessary have to correspond to some physical path on the hard drive: in
the end, the user of the task pipeline (the one who runs the executable) will
bind each logical path to a physical location. Besides, a `VirtualFile` doesn't
even have to correspond in the end to an actual file, as for instance you can
map an entry in a database to a `VirtualFile`. However, paths are a convenient
and customary way to organise resources, and we can conveniently use them as a
default layout for when your logical paths do correspond to actual paths on your
hard drive.

So once the user has their serials, they just need to create a
`VirtualFile`. For instance, this is how you create a readonly resource that can
only be mapped to a JSON file in the end:

```haskell
myInput :: VirtualFile Void MyConfig
	-- MyConfig must be an instance of FromJSON here
myInput = dataSource
            ["Inputs", "Config"] -- The logical path '/Inputs/Config'
	        (somePureDeserial JSONSerial)

somePureDeserial :: (DeserializesWith s a) => s -> SerialsFor Void a
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
```

## Tasks

A `PTask` is an arrow, that is to say a computation with an input and an
output. Here we just call these computation "tasks". PTasks run in a base monad
`m` that can depend on the application but that should always implement
`KatipContext` (for logging), `MonadCatch`, `MonadResource` and `MonadUnliftIO`.

This is how we create a task that reads the `myInput` VirtualFile we defined
previously:

```haskell
readMyConfig :: (KatipContext m, MonadThrow m)
	         => PTask m () MyConfig
  -- The input of the task is just (), as we are just reading something
readMyConfig = loadData myInput
```

This task reads the actual physical location bound to `/Input/Config` and
returns its content as a value of type `MyConfig`.

So `PTasks` can access resources or perform computations, as any pure function
`(a -> b)` can be lifted to a `PTask m a b`. Each `PTask` will expose the
VirtualFiles it accesses (we call them the _requirements_ of the task, as it
requires these files to be present and bound to physical locations so it can
run) in the form of a resource tree. `PTasks` compose much like functions do,
and they merge their requirements as they compose, so in the end if you whole
application runs in a `PTask`, then it will expose and make bindable the
totality of the resources accessed by your application.

Once you have e.g. a `mainTask :: PTask () ()` that corresponds to your whole
pipeline, your application just needs to call:

```haskell
main :: IO ()
main = runPipelineTask cfg mainTask ()
  where
    cfg = FullConfig "MyApp" "pipeline-config.yaml" "./default-root-dir"
```

## Running a Porcupine application

Once you have built your executable, what you will usually want is to expose its
configuration. Just run it with:

```sh
$ my-exe write-config-template
```

if your `main` looks like the one we presented previously, that will generate a
`pipeline-config.yaml` file in the current directory. In this file, you will see
the totality of the virtual files accessed by your pipeline and the totality of
the options^[Options are just VirtualFiles, but they are created with the
`getOptions` primitive task, and their values can be embedded directly in the
configuration file] exposed by it. You can see that by default, the root (`"/"`)
of the location tree is mapped to `./default-root-dir`. If you leave it as it
is, then every VirtualFile will be looked for under that directory, but you can
alter that on a per-file basis.

Once you're done tweaking the configuration, just call:

```sh
$ my-exe run
```

and the pipeline will run (logging its accesses along). The `run` is optional,
it's the default subcommand. Any option you defined inside your pipeline is also
exposed on the CLI, and shown by `my-exe --help`. Specifying it on the CLI
overrides the value set in the yaml config file.

# Philosophy of use

Porcupine's intent is to make it easy to cleanly separate the work between 3
persons:

- The _storage developer_ will be in charge of determining how the data gets read
  and written in the end. He will target the _serials_ framework, and propose
  new datatypes (data frames, matrices, vectors, trees, etc.) and ways to write
  and read them to the various storage technologies.
- The _scientist_ will determine how to carry out the data analyses, extract some
  sense out of the data, run simulations based on that data, etc. He doesn't
  have to know how the data is represented, just that it exists. She just reuses
  the serials written by the storage developper and targets the _tasks_
  framework.
- The _software architect_ work will start once we need to bump things up a
  bit. Once we have iterated several times over our analyses and simulations and
  want to have things running in a bigger scale, then it's time for the pipeline
  to move from the scientist's puny laptop and go bigger. This is time to
  "patch" the pipeline, make it run in different context, in the cloud, behind a
  scheduler, as jobs in a task queue reading its inputs from all kinds of
  databases. The software architect will target the _resource tree_ framework
  (possibly without ever recompiling the pipeline, only by adjusting its
  configuration from the outside)

Of course, these people can be the same person, and you don't need to plan on
runnning anything in the cloud to start benefiting from porcupine. But we want
to support workflows where these three persons are distinct people, each one
with her different set of skills.


# Specific features

Aside from the general usage exposed previously, porcupine proposes several
features to facilitate iterative development of pipelines and reusability of
tasks.

[TO BE FILLED]

## Options and embeddable data

## Location layers

## Repeated tasks and VirtualFiles

## Mapping S3 objects

## Control logging

Porcupine uses `katip` to do logging. It's quite a versatile tool, and we
benefit from it. By default, logging uses a custom readable format. You can
switch to more standard formats using:

```sh
$ my-exe --log-format json
```

## Create a custom serials bank

## Adding pipeline wrappers
