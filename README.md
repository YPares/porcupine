# Porcupine

Porcupine stands for _Portable, Reusable & Customizable Pipeline_. It is a tool
aimed at data-scientists and numerical analysts, so they can express general
data manipulation and analysis tasks in a way (1) that is agnostic from the
source of the input data and from the destination of the end results, (2) that a
pipeline can be re-executed in a different environment and on different data
without recompiling, by
just a shift in its configuration and (3) that composability is maintained (any
task can always be reused as a subtask of a greater task pipeline).

Porcupine provides three frameworks that work together in that end: _serials_,
_tasks_ and _resource trees_.

## Serials

A `SerialsFor a b` encompasses functions to write data of type `a` and read data
of type `b`. Porcupine provides a few serials if your datatype already
implements standard serialization interfaces, such as `aeson`'s To/FromJSON or
`binary`, and makes it easy to reuse custom serialization functions you might
already have.

`SerialsFor` is a profunctor. That means that once you know how to (de)serialize
an `A` (ie. if you have a `SerialsFor A A`), then you can just use `dimap` to
get a `SerialsFor B B` if you know how to convert `A` to & from `B`. Handling
only one-way serialization or deserialization is perfectly possible, that just mean you
will have `SerialsFor Void B` or `SerialsFor A ()`. Any `SerialsFor a b` is also
a monoid, meaning that you can for instance gather default serials, or serials
from an external source and add to them your custom serialization methods,
before using it in a task pipeline.

The end goal of `SerialsFor` is that the user writing a task pipeline will not
have to care about how the input data will be serialized. As long as the data it
tries to feed into the pipeline matches some known serialization function. Also,
the introspectable nature of resource trees (more on that later) allows you to
_add_ serials to an existing pipeline before reusing it as part of your own
pipeline. This sort of makes Porcupine an "anti-ETL": rather than marshall and
curate input data so that it matches the pipeline expectations, you augment the
pipeline so that it can deal with more different data sources.

## Resource tree

Every task in Porcupine exposes a resource tree. This a morally a hierarchy of
`VirtualFile`s, which the end user of the task pipeline can bind to physical
locations. However this tree isn't created manually by the developper of the
pipeline, it's completely hidden from them. This tree is made of atomic bits
(constructed by the primitive tasks) which are composed when tasks are composed
together to create the whole pipeline.

Once the user has their serials, they just need to create a `VirtualFile` (a
thin layer over `SerialsFor`, which is also a profunctor). For instance, this is
how you create a readonly resource that can only be mapped to a JSON file in the
end:

```haskell
myInput :: VirtualFile Void MyType
	-- MyType must be an instance of FromJSON here
myInput = dataSource ["Inputs", "MyInput"]
	                 (somePureDeserial JSONSerial)
```

And then, using `myInput` in a task pipeline is just a matter of calling the
primitive task `accessVirtualFile myInput`. `accessVirtualFile` just turns a
`VirtualFile a b` into a `PTask a b`.

## Tasks

A `PTask` is an arrow. Atomic `PTask`s can access resources, as we saw, or
perform computations, as any pure function can be lifted. Each `PTask` will
expose is requirements in the form of a resource tree, and a function that will
actually execute the task when the pipeline runs. `PTask`s compose much like
functions do, and they merge their requirements as they go.

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


# Specific features

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
