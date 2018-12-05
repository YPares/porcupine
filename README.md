# Porcupine

[![CircleCI](https://circleci.com/gh/tweag/porcupine/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/porcupine/tree/master)

Porcupine stands for _Portable, Reusable & Customizable Pipeline_. It
is a tool aimed at data scientists and numerical analysts, so that
they can express general data manipulation and analysis tasks,

1. in a way that is agnostic from the source of the input data and
from the destination of the end results,
2. such that a pipeline can be re-executed in a different environment
and on different data without recompiling, by just a shift in its
configuration,
3. while maintaining composability (any task can always be reused as
a subtask of a greater task pipeline).

Porcupine provides three core abstractions: _serials_, _tasks_ and
_resource trees_.

## Serials

A `SerialsFor a b` encompasses functions to write data of type `a` and read data
of type `b`. Porcupine provides a few serials if your datatype already
implements standard serialization interfaces, such as `aeson`'s `To/FromJSON` or
`binary`, and makes it easy to reuse custom serialization functions you might
already have.

TODO: What's the type of the encoding and decoding functions? `ByteString -> b`
and `a -> ByteString`? Probably related to my confusion better explained in the
following TODO.

TODO: Explain what a `serial` is?

`SerialsFor` is a [profunctor][profunctor]. That means that once you know how to (de)serialize
an `A` (ie. if you have a `SerialsFor A A`), then you can just use `dimap` to
get a `SerialsFor B B` if you know how to convert `A` to & from `B`. Handling
only one-way serialization or deserialization is perfectly possible, that just mean you
will have `SerialsFor Void B` or `SerialsFor A ()` and use only `lmap` or `rmap`. Any `SerialsFor a b` is also
a monoid, meaning that you can for instance gather default serials, or serials
from an external source and add to them your custom serialization methods,
before using it in a task pipeline.

TODO: If `SerialsFor A B` is a monoid, what's the meaning of `<>` and `empty`?
This is the first time it surfaces that a `SerialsFor` does not wrap a function
to encode and a function to decode, it seems to wrap multiple functions to encode
and multiple functions to decode. It needs to be better conveyed. Also I have
no intuition on how the encoding/deconding functions are meant to be picked for
a particular use.

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

Every task in Porcupine exposes a resource tree. This a morally a hierarchy of
`VirtualFiles`, which the end user of the task pipeline (the one who runs the executable)
can bind to physical
locations. However this tree isn't created manually by the developper of the
pipeline, it's completely hidden from them. This tree is made of atomic bits
(constructed by the primitive tasks) which are composed when tasks are composed
together to create the whole pipeline.

TODO: I don't get what a `VirtualFile` is. The paragraph makes it sound as a
resource parameter of the pipeline. The paragraph below adds that it is a thin
layer over `SerialFor`, which doesn't help either. In either case, it is not
clear what is virtual-like and what is file-like in a `VirtualFile`.

TODO: Some concepts haven't been introduced yet in the above paragraph.
I don't know what a "primitive task" or an "atomic bit" is. I'm wondering
if this section would work better after the one about tasks.

TODO: If the tree is hidden from the programmer, is it an implementation detail?
The paragraph is not making clear why should she care of a hierarchy of `VirtualFiles`.

TODO: Explain what is the relation between VirtualFiles which constitutes the
hierarchy? The last part of the paragraph seems to hint that a subtask
relation is involved, but this relates tasks, not VirtualFiles.

Once the user has their serials, they just need to create a `VirtualFile` (a
thin layer over `SerialsFor`, which is also a profunctor). For instance, this is
how you create a readonly resource that can only be mapped to a JSON file in the
end:

```haskell
myInput :: VirtualFile Void MyType
	-- MyType must be an instance of FromJSON here
myInput = dataSource
            ["Inputs", "MyInput"] -- A virtual path
	        (somePureDeserial JSONSerial)
```

TODO: Show type signatures of `dataSource` and `somePureDeserial`?

And then, using `myInput` in a task pipeline is just a matter of calling the
primitive task `accessVirtualFile myInput`, and the whole pipeline will expose
a new `/Inputs/MyInput` virtual file. `accessVirtualFile` just turns a
`VirtualFile a b` into a `PTask a b`.

TODO: Not knowing what a `PTask` is, and what the meaning is of having a
pipeline expose a virtual file, this paragraph should probably be removed
or placed later.

## Tasks

A `PTask` is an arrow. Atomic `PTasks` can access resources, as we saw, or
perform computations, as any pure function can be lifted. Each `PTask` will
expose its requirements (in the form of a resource tree) and a function that will
actually execute the task when the pipeline runs. `PTasks` compose much like
functions do, and they merge their requirements as they compose.

TODO: Explain that an arrow is a computation with an input and an output.

TODO: What means that a PTask is atomic? I'm guessing that it means it is
not built from composing other PTasks. It is possibly best to choose one of
primitive and atomic to refer to these tasks consistently (primitive could
be better if this has nothing to do with atomicity in transactions).

TODO: Provide a an overview of the major groups of primitive operations that
are avaialble.

Once you have e.g. a `mainTask :: PTask () ()` that corresponds to your whole
pipeline, your application just needs to call:

```haskell
main :: IO ()
main = runPipelineTask "myApp" cfg mainTask ()
  where
    cfg = FullConfig "pipeline-config.yaml" "./default-root-dir"
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
