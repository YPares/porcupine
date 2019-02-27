# Porcupine

[![CircleCI](https://circleci.com/gh/tweag/porcupine/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/porcupine/tree/master) [![Join the chat at https://gitter.im/tweag/porcupine](https://badges.gitter.im/tweag/porcupine.svg)](https://gitter.im/tweag/porcupine?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Porcupine stands for _Portable & Customizable Pipeline_. It is a tool
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
output. Here we just call these computations "tasks". PTasks run in a base monad
`m` that can depend on the application but that should always implement
`KatipContext` (for logging), `MonadCatch`, `MonadResource` and `MonadUnliftIO`.
However you usually don't have to worry about that, as porcupine takes care of these
dependencies for you.

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
main = runPipelineTask_ cfg mainTask
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
- The _devops_ work will start once we need to bump things up a
  bit. Once we have iterated several times over our analyses and simulations and
  want to have things running in a bigger scale, then it's time for the pipeline
  to move from the scientist's puny laptop and go bigger. This is time to
  "patch" the pipeline, make it run in different context, in the cloud, behind a
  scheduler, as jobs in a task queue reading its inputs from all kinds of
  databases. The devops will target the _resource tree_ framework
  (possibly without ever recompiling the pipeline, only by adjusting its
  configuration from the outside)

Of course, these people can be the same person, and you don't need to plan on
runnning anything in the cloud to start benefiting from porcupine. But we want
to support workflows where these three persons are distinct people, each one
with her different set of skills.

# Walking through an example

Let's have a look at [example1](porcupine-core/examples/Example1.hs). It carries
out a very simple analysis: counting the number of times each letter of the
alphabet appears in some users' first name and last name. Each user data is read
from [a json file specific to that user](porcupine-core/examples/data/Inputs)
named `User-{userId}.json`, and the result for that user is written to another
json file named `Analysis-{userId}.json`. This very basic process is repeated
once per user ID to consider.

Let's build and run that example. In a fresh clone and from the root directory
of `porcupine`:

```sh
$ stack build porcupine-core:example1
$ stack exec example1 -- write-config-template
```

You will see that it wrote a `porcupine.yaml` file in the current folder. Let's
open it:

```yaml
variables: {}
data:
  Settings:
    users: 0
  Inputs: {}
  Outputs: {}
locations:
  /Inputs/User: _-{userId}.json
  /: porcupine-core/examples/data
  /Outputs/Analysis: _-{userId}.json
```

The `data:` section contains one bit of information here: the `users:` field,
corresponding to ranges of user IDs to consider. By default, we just consider
user #0. But you could write something like:

```yaml
    users: [0..10,50..60,90..100]
```

Meaning that we'd process users from #0 to #10, then users from #50 to #60, then
users from #90 to #100.

You can alter the YAML and then run the example with:

```sh
$ stack exec example1
```

And you'll see it computes and writes an analysis of all the required users
(although it will fail if you ask for user IDs for which we do not have input
files). Have a look at the output folder (`porcupine-core/examples/data/Outputs`
by default).

Then you see the `locations:` section. Look at the `/:` location, it's the
default root under which every file we be looked for and written, and `example1`
by default binds it to `porcupine-core/examples/data`.

Let's have a look at the other two lines:

```yaml
  /Inputs/User: _-{userId}.json
  /Outputs/Analysis: _-{userId}.json
```

You can see here the default mappings for the two virtual files `/Inputs/User`
and `/Ouputs/Analysis` that are used by `example1`. Wait. TWO virtual files? But
we mentioned earlier that there were *many* user files to read and just as many
analysis files to write? Indeed, but these are just *several occurences* of the
same two VirtualFiles. You'll see in the code of example1 that the task
conducing the analysis on one user doesn't even know that several users might
exist, let alone where these users' files are. It just says "I want a user" (one
VirtualFile, to be read) and "I output an analysis" (one VirtualFile, to be
written). But more on that later.

You can also notice that there is no difference between input and output files
here. The syntax to bind them to physical locations is the same. Although the
syntax of the mappings probably requires a bit of explaining:

- The underscore sign at the beginning of a physical location means we want to
  inherit the path instead of specifying it entirely. It means that the path
  under which we will look for the file is to be derived from the rest of the
  bindings:
    - `/Inputs/User` is "located" under the virtual folder `/Inputs`
	- `/Inputs` isn't explicitly mapped to any physical folder here. However,
	    it is itself "located" under `/`
	- `/` is bound to a physical folder, `porcupine-core/examples/data`
	- then `/Inputs` is bound to `porcupine-core/examples/data/Inputs`
	- and `/Inputs/User` is bound to
	    `porcupine-core/examples/data/Inputs/User-{userId}.json`.
- As you may have guessed, the `{...}` syntax denotes a _variable_, it's a part
  of the file path that will depend on some context (here, the ID of the user
  that is being analyzed).

Let's know have a look at the code of
[`example1`](porcupine-core/examples/Example1.hs) (types have been eluded for
brevity but they are in the source):

```haskell
userFile = dataSource ["Inputs", "User"]
                      (somePureDeserial JSONSerial)

analysisFile = dataSink ["Outputs", "Analysis"]
                        (somePureSerial JSONSerial)

computeAnalysis (User name surname _) = Analysis $
  HM.fromListWith (+) $ [(c,1) | c <- T.unpack name]
                     ++ [(c,1) | c <- T.unpack surname]

analyseOneUser =
  loadData userFile >>> arr computeAnalysis >>> writeData analysisFile
```

We declare our source, our sink, a pure function to conduct the analysis, and
then a very simple pipeline that will bind all of that together.

At this point, if we were to directly run `analyseOneUser` as a task, we
wouldn't be able to make it run over _all_ the users. As is, it would try to access
`porcupine-core/examples/data/Inputs/User.json` (notice the variable `{userId}`
is missing) and write `Outputs/Analysis.json`.

This is not what we want. That's why we'll declare another task on top of that:

```haskell
mainTask =
  getOption ["Settings"] (docField @"users" (oneIndex (0::Int)) "The user ids to load")
  >>> arr enumIndices
  >>> parMapTask_ (repIndex "userId") analyseOneUser
```

Here, we will first declare a record of one option field (that's why we use
`getOption` here, singular). This record is also a VirtualFile we want to read,
so it has to have a virtual path (here, `/Settings`). The field is created with
`docField` (notice that the extension `TypeApplications` is used here), and, as
we saw earlier in the yaml config, it is named `users` and has a default value
of just one ID, zero. `oneIndex` returns a value of type `IndexRange`, and
therefore that's what `getOption` will give us. But really any type could be
used here, as long as this type instanciates `ToJSON` and `FromJSON` (so we can
embed it in the YAML config file).

`getOption`'s output will thus be list of ranges (tuples). We transform it to a
flat list of every ID to consider with `enumIndices`, and finally we map (in
parallel) over that list with `parMapTask_`, repeating our `analyseOneUser` once
per user ID in input. Note the `repIndex "userId"`: this creates a *repetition
index*. Every function that repeats tasks in porcupine will take one, and this
is where the `{userId}` variable that we saw in the YAML config comes from. It
works first by changing the default mappings of the VirtualFiles accessed by
`analyseOneUser` so that each of them now contains the `{userId}` variable, and
second by looking up that variable in the physical path and replacing it with
the user ID currently being considered. All of that so we don't end up reading
and writing the same files everytime the task is repeated.

So what happens if you remove the variables from the mappings? Let's say you
change the mappings of `/Outputs/Analysis` to:

```yaml
  /Outputs/Analysis: ./analysis.json
```

well then, this means that the same file will be overwritten again and again. So
you definitely don't want that.

However, you can perfectly move the variable any place you want. For instance
you can reorganize the layout of the inputs and outputs to have one folder per
user, with inputs and ouputs put side to side:

```
Users/
├── 0/
│   ├── Analysis.json
│   └── User.json
├── 1/
|   ├── Analysis.json
|   └── User.json
├── 2/
...
```

The configuration to take into account that new layout is just the following:

```yaml
locations:
  /Inputs/User: Users/{userId}/User.json
  /Outputs/Analysis: Users/{userId}/Analysis.json
```

In that scheme, the `Inputs`/`Ouputs` level has disappeared, and the
VirtualFiles specify their paths explicitely.

# Specific features

Aside from the general usage exposed previously, porcupine proposes several
features to facilitate iterative development of pipelines and reusability of
tasks.

## Options and embeddable data

To hammer a bit on porcupine's philosophy, our goal is that there should be the
least possible differences between a pipeline's _inputs_ and its
_configuration_. Every call to `getOptions` will internally declare a
VirtualFile, and is only a thin layer on top of `loadData`. That means that a
	record of options passed to `getOptions` will be treated like any other input,
and can perfectly be moved to a dedicated JSON or YAML file instead of being
stored in the `data:` section of the pipeline configuration file.

Conversely, any `VirtualFile` that uses the JSONSerial can be directly embedded
in the `data:` section. Don't forget in that case to map that VirtualFile to
`null` in the `locations:` section, or else the pipeline will try to access a
file that doesn't exist.


## Location layers

## Repeated tasks and VirtualFiles

## Location accessors

## Control logging

Porcupine uses `katip` to do logging. It's quite a versatile tool, and we
benefit from it. By default, logging uses a custom readable format. You can
switch to more standard formats using:

```sh
$ my-exe --log-format json
```

## Create a custom serials bank

## Adding pipeline wrappers
