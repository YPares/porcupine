# Porcupine

![logo](porcupine.svg)
[![CircleCI](https://circleci.com/gh/tweag/porcupine/tree/master.svg?style=svg)](https://circleci.com/gh/tweag/porcupine/tree/master)
[![Join the chat at
https://gitter.im/tweag/porcupine](https://badges.gitter.im/tweag/porcupine.svg)](https://gitter.im/tweag/porcupine?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


"porcupine" stands for _Portable & Customizable Pipeline_. It is a tool aimed at
people who want to express in Haskell general data manipulation and analysis tasks,

1. in a way that is agnostic from the source of the input data and from the
destination of the end results,
2. such that a pipeline can be re-executed in a different environment and on
different data without recompiling, by just a shift in its configuration,
3. while facilitating code reusability (any task can always be reused as part
of a bigger pipeline).

`porcupine` specifically targets teams containing skills ranging from those of data scientists
to those of data/software engineers.

## First example

The following example does something very stupid. We want to generate a text
file containing n times the letter 'a', n being a parameter given by the user:

```haskell
generateTxt :: Int -> T.Text -- A pure function generating the text
generateTxt n = T.replicate n "a"

serialMethod :: SerialsFor T.Text NoRead -- The serialization method
serialMethod = somePureSerial (PlainTextSerial (Just "txt"))
	-- Here the serialization method is very stupid, as it's basically just identity

resultFile :: VirtualFile T.Text NoRead  -- A sink in which we will write a Text
resultFile = dataSink ["result"] serialMethod
    -- The sinks knows about our serialization method

myTask :: (LogThrow m) => PTask m () ()  -- The task that performs all the operations
myTask =
      getOption ["options"]  -- First we request value for a parameter
                (docField @"replications" (10::Int)
	     -- The param is named "replications" and will by default have a value of 10
                          "The length of the text to output")
  >>> arr generateTxt -- Then we create a text of that length
  >>> writeData resultFile -- And finally we write it in the sink
```

This example is located [here](porcupine-core/examples/example0/Example0.hs). Run it with

```
$ stack build porcupine-core:example0
$ stack exec example0
```

This should create a file `./result.txt`.

### Changing the parameters

The "replications" parameter has automatically been exposed via the command-line
arguments. So for instance you can run

```
$ stack exec example0 -- --replications 100
```

and see that `./result.txt` now contains 100 characters.

Another thing you can already do is remap (redirect) our sink to another file:

```
$ stack exec example0 -- -l /result=other-result.txt
```

and see that the program now writes to `./other-result.txt` instead of
`./result.txt`. `-l` (or `--loc`) is one of the default CLI options of
`porcupine`, present in all the executables compiled with CLI support.

### Showing the dependencies and parameters of our program

One final thing we can do on this example is print the tree of sinks, sources
and options that it uses:

```
$ stack exec example0 -- show-tree -m
```

this should print:

```
/: 
|
+- result: ./result.txt
|    DATA SINK
|    Accepts txt
|
`- options: <no mapping>
     OPTION SOURCE (embeddable)
     Accepts yaml, json, yml
     
   --- Fields ---
   text-length :: Int : The length of the text to output
```

Where you can see the resources that our program needs, their default mappings
to physical files, and the option field we declared with its type and docstring.

This example is extremely contrived, but it already shows the main tools that
`porcupine` provides to build a data pipeline and interact with it: _serials_,
_virtual files_, _tasks_ and _trees_.

## Serials

A `SerialsFor a b` encompasses functions to write data of type `a` and read data
of type `b`. Porcupine provides a few serials if your datatype already
implements standard serialization interfaces, such as `aeson`'s `To/FromJSON` or
`binary`, and makes it easy to reuse custom serialization functions you might
already have. A `SerialsFor A B` is a collection of `A -> i` and `i -> B`
functions, where `i` can be any intermediary type, most often `ByteString`,
`Data.Aeson.Value` or `Text`.

The end goal of `SerialsFor` is that the user writing a task pipeline will not
have to care about how the input data will be serialized. As long as the data it
tries to feed into the pipeline matches some known serialization function. Also,
the introspectable nature of virtual trees (more on that later) allows you to
_add_ serials to an existing pipeline before reusing it as part of your own
pipeline. This sort of makes Porcupine an
"anti-[ETL](https://en.wikipedia.org/wiki/Extract,_transform,_load)": rather
than marshall and curate input data so that it matches the pipeline
expectations, you augment the pipeline so that it can deal with more data
sources.

If you are experienced with Haskell typeclasses and abstractions, it is
interesting to note that `SerialsFor` is a [profunctor]. This means that once
you know how to (de)serialize an `A` (ie. if you have a `SerialsFor A A`), then
you can just use `dimap` to get a `SerialsFor B B` if you know how to convert
`A` to & from `B`. Handling only one-way serialization or deserialization is
perfectly possible, that just mean you will have `SerialsFor NoWrite B` or
`SerialsFor A NoRead` and use only `lmap` or `rmap`. A `SerialsFor a b` is also
a semigroup, where `(<>)` merges together the collections of serialization
functions they contain, meaning that you can for instance gather default
serials, or serials from an external source and add to them your custom
serialization methods, before using it in a task pipeline.

`NoWrite` and `NoRead` are equivalent respectively to `Void` and `()`. We use
them for clarity reasons, and also to avoid orphan instances.

[profunctor]: https://www.stackage.org/haddock/lts-12.21/lens-4.16.1/Control-Lens-Combinators.html#t:Profunctor

## Porcupine's trees of virtual files

Every task in Porcupine exposes a _virtual tree_. A virtual tree is a hierarchy
(like a filesystem) of `VirtualFiles`. A `VirtualFile A B` just groups together
a logical path and a `SerialsFor A B`, so it is just something with an
identifier (like `"/Inputs/Config"` or `"/Ouputs/Results"`) in which we can
write a `A` and/or from which we can read a `B`. We say the path is "logical"
because it doesn't necessarily have to correspond to some physical path on the
hard drive: in the end, the user of the task pipeline (the one who runs the
executable) will bind each logical path to a physical location. Besides, a
`VirtualFile` doesn't even have to correspond in the end to an actual file, as
for instance you could map an entry in a database to a `VirtualFile`. However,
paths are a convenient and customary way to organise resources, and we can
conveniently use them as a default layout for when your logical paths do
correspond to actual paths on your hard drive.

Most often we use the aliases `DataSource` and `DataSink` instead of using
directly `VirtualFile`.

This is how you create a readonly resource that can be mapped to a JSON or YAML
file in the end:

```haskell
myInput :: DataSource MyConfig  -- DataSource is just `VirtualFile NoWrite`
	-- MyConfig must be an instance of FromJSON here
myInput = dataSource
            ["Inputs", "Config"] -- The logical path '/Inputs/Config'
            (somePureDeserial JSONSerial)

somePureDeserial :: (DeserializesWith s a) => s -> SerialsFor NoWrite a
dataSource :: [LocationTreePathItem] -> SerialsFor a b -> DataSource b
```

Later on, before the pipeline runs, your `VirtualTree` will be resolved to
actual locations and will become a `DataAccessTree`.

## Tasks

A `PTask` is a computation with an input and an output. Here we just call these
computations "tasks". PTasks run in a base monad `m` that can depend on the
application but that should always implement `KatipContext` (for logging),
`MonadCatch`, `MonadResource` and `MonadUnliftIO`. However you usually don't
have to worry about that, as porcupine takes care of these dependencies for you.

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
run) in the form of a virtual tree. `PTasks` are
[arrows](https://wiki.haskell.org/Arrow_tutorial), and as such they compose much
like functions do. The main difference with functions is that tasks merge their
requirements as they compose. So in the end if you whole application runs in a
`PTask`, then it will expose and make bindable the totality of the resources
accessed by your application. A `PTask` _exposes_ a `VirtualTree` as its
requirements, and when it actually runs it _receives_ a `DataAccessTree` that
contains the functions to actually pull the data it needs or output the data it
generates. Both trees are editable in the task, so a `PTask` can be altered from
the outside, in order to adapt it to work in a different context than the one it
was conceived for, which boosts code reusability.

Once you have e.g. a `mainTask :: PTask () ()` that corresponds to your whole
pipeline, your application just needs to call:

```haskell
main :: IO ()
main = runLocalPipelineTask cfg mainTask
  where
    cfg = FullConfig "MyApp" "pipeline-config.yaml" "./default-root-dir" ()
```

## Running a Porcupine application

We saw in our [first example](#first-example) that the parameters of our tasks
are exposed via the command-line. But it isn't the only source of configuration
you can use. You can also create a YAML or JSON file that will hold that
configuration. Once you have built your executable, run:

```sh
$ my-exe write-config-template
```

If your `main` looks like the one we presented previously, that will generate a
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
it's the default subcommand. As we saw any option you defined inside your
pipeline is also exposed on the CLI, and shown by `my-exe --help`. Specifying it
on the CLI overrides the value set in the YAML config file.

A note about the `FullConfig` option you saw earlier. Porcupine is quite a
high-level tool and handles configuration file and CLI parsing for you, and this
is what `FullConfig` triggers. But this is not mandatory, you also have
`ConfigFileOnly` for when you don't want porcupine to parse the CLI arguments
(this is for the cases when your pipeline isn't the entirety of your program, or
when you want for some reason to parse CLI yourself) and `NoConfig` for when you
don't want any source of configuration to be read. When CLI parsing is
activated, you need to provide a default value for the pipeline: indeed using
`write-config-template` means your pipeline won't even run.

# Philosophy of use

Porcupine's intent is to make it easy to separate clearly the work between 3
persons:

- The _software developer_ will be in charge of determining how the data gets
  read and written in the end. He will target the _serials_ framework, and
  propose new datatypes (data frames, matrices, vectors, trees, etc.) and ways
  to write and read them to the various storage technologies.
- The _scientist_ will determine how to carry out the data analyses, extract
  some sense out of the data, run simulations based on that data, etc. He
  doesn't have to know how the data is represented, just that it exists. She
  just reuses the serials written by the developer and targets the _tasks_
  framework.
- The _devops_ work will start once we need to bump things up a bit. Once we
  have iterated several times over our analyses and simulations and want to have
  things running in a bigger scale, then it's time for the pipeline to move from
  the scientist's puny laptop and go bigger. This is time to "patch" the
  pipeline, make it run in different context, in the cloud, behind a scheduler,
  as jobs in a task queue reading its inputs from all kinds of databases. The
  devops will target the _porcupine tree_ framework (possibly without ever
  recompiling the pipeline, only by adjusting its configuration from the
  outside). But more on that later.

Of course, these can be the same person. Also, you don't need to plan on running
anything in the cloud to start benefiting from porcupine. But we want to support
workflows where these three persons are distinct people, each one with her
different set of skills.

# Walking through an example

Let's now have a look at
[example1](porcupine-core/examples/example1/Example1.hs). It carries out a
simple analysis: it counts the number of times each letter of the alphabet
appears in some users' first name and last name. Each user data is read from [a
json file specific to that user](porcupine-core/examples/example1/data/Inputs)
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

You can compare it to the output of `show-tree`:

```sh
$ stack exec example1 -- show-tree
```

```
/: 
|
+- Settings: OPTION SOURCE (embeddable)
|    Accepts yaml, json, yml
|
|  --- Fields ---
|  users :: IndexRange Int : The user ids to load
|
+- Inputs: 
|  |
|  `- User: DATA SOURCE (embeddable) repeated over "userId"
|       Accepts json, yml, yaml
|
`- Outputs: 
   |
   `- Analysis: DATA SINK repeated over "userId"
        Accepts json, yml, yaml
```

You can use `show-tree -m` to additionally show the mappings.

This hierarchy of virtual paths filled with data and mappings to physical paths
to resources is what we call the _porcupine tree_ of the pipeline.

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

Another option you have is leave the yaml file like it is and just run:

```yaml
stack exec example1 -- --users "[0..10,50..60,90..100]"
```

Most of the options you can see under `data:` in a pipeline's configuration will
also have a CLI flag counterpart. Run the executable with `--help` to see them.

Back to the yaml file now. You see the `locations:` section. Look at the `/:`
location, it's the default root under which every file we be looked for and
written, and `example1` by default binds it to `porcupine-core/examples/data`.

Let's have a look at the other two lines:

```yaml
  /Inputs/User: _-{userId}.json
  /Outputs/Analysis: _-{userId}.json
```

You can see here the default mappings for the two virtual files `/Inputs/User`
and `/Ouputs/Analysis` that are used by `example1`. Wait. TWO virtual files? But
we mentioned earlier that there were *many* user files to read and just as many
analysis files to write? Indeed, but these are just *several occurences* of the
same two VirtualFiles. As the output of `show-tree` mentions it these virtual
files are indeed _repeated_. You'll see in the code of example1 that the task
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
	- `/Inputs` isn't explicitly mapped to any physical folder here. However, it
	    is itself "located" under `/`
	- `/` is bound to a physical folder, `porcupine-core/examples/data`
	- then `/Inputs` is bound to `porcupine-core/examples/data/Inputs`
	- and `/Inputs/User` is bound to
	    `porcupine-core/examples/data/Inputs/User-{userId}.json`.
- As you may have guessed, the `{...}` syntax denotes a _variable_, it's a part
  of the file path that will depend on some context (here, the ID of the user
  that is being analyzed).

Let's know have a look at the code of
[`example1`](porcupine-core/examples/example1/Example1.hs) (types have been eluded for
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
wouldn't be able to make it run over _all_ the users. As is, it would try to
access `porcupine-core/examples/data/Inputs/User.json` (notice the variable
`{userId}` is missing) and write `Outputs/Analysis.json`.

This is not what we want. That's why we'll declare another task on top of that:

```haskell
mainTask =
  getOption ["Settings"] (docField @"users" (oneIndex (0::Int)) "The user ids to load")
  >>> arr enumIndices
  >>> parMapTask_ "userId" analyseOneUser
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
per user ID in input. Note the first argument `"userId"`: this is the
*repetition index* (you need OverloadedStrings). Every function that repeats
tasks in porcupine will take one, and this is where the `{userId}` variable that
we saw in the YAML config comes from. It works first by changing the default
mappings of the VirtualFiles accessed by `analyseOneUser` so that each of them
now contains the `{userId}` variable, and second by looking up that variable in
the physical path and replacing it with the user ID currently being
considered. All of that so we don't end up reading and writing the same files
everytime the task is repeated.

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
VirtualFiles specify their paths explicitly.

# Specific features

Aside from the general usage exposed previously, porcupine proposes several
features to facilitate iterative development of pipelines and reusability of
tasks.

## Splicing variables in physical paths

We already saw in [this section](#walking-through-an-example) that the physical
paths specified in the configuration need not correspond _fully_ to physical
paths. Sometimes, these paths will contain one or several _variables_ to be
spliced if the task which reads/writes these paths will be repeated several
times over a set of _task repetition indices_ (TRIndices), which can be as
simple as an integer growing from 0 to some number. In such case, the variable
name in the path will be everytime replaced by the current value of the TRIndex.

You can also add variables in a configuration file even in the absence of
repeated tasks, to gain some very light templating capabilities, for instance:

```yaml
variables:
  folder: experiment1
data:
  Settings:
    users: 0
  Inputs: {}
  Outputs: {}
locations:
  /Input: data/{folder}/input.json
  /Output: data/{folder}/output.json
```

In this case, `{folder}` will just be replaced by `experiment1` by default. This
makes it so you only have to change once the `folder` variable when you want to
target another folder. Also, you can set the variable's value with CLI:

```
my-exe --var folder=experiment143
```

This is convenient when you often want to quickly change some paths, but if you
need more complex templating then we suggest you use a full-fledged solution,
like [jsonnet](https://jsonnet.org/).

## Ahead-of-time sanity checks and overview of resources

`porcupine` allows you to have a preliminary view of _every_ resource needed at
some point by your pipeline, every parameter customizable in you tasks, before
anything needs to run. This is extremely powerful as this permits a much, much
larger amount of checks to be carried out ahead of time. Notably `porcupine`
doesn't allow a virtual file to be mapped to a physical file that doesn't have
an expected extension.

The main tool you can use to gain insight on you pipeline is the `show-tree`
subcommand we already saw. It accepts a few flags:

- `-m`: Show the mappings
- `-t`: Show the datatypes written/read
- `-a`: Show the accesses that will be performed (you can know in advance
  eg. how many times a file will be read)
- `-S`: Don't show the SOURCE/SINK labels
- `-E`: Don't show the filetypes (extensions) the virtual file accepts
- `-F`: Don't show the option fields that are contained is this virtual file

Additionally, `show-tree /data/stuff` will show you only the part of the
porcupine tree starting from path `/data/stuff`.

In the future, we plan on adding new, optional ahead of time sanity checks that
could be performed with a specific subcommand, like:

- Checking the existence of all files,
- Performing a dry run of the serials to verify that all inputs are correctly
  formatted,
- Checking that every server we will try to write to is actually attainable.

And given a `PTask` exposes its `VirtualTree` via the `taskRequirements` lens,
you can even perform you own sanity checks before calling `runPipelineTask`.

## Configuration, data embedded in config file, and external input files/resources

Let's say you have a pipeline that needs an input file to do some simulation,
and a record of options controlling the way the simulation happens (which solver
to use, which parameters to give it, etc). What exactly is the _input_ and what
exactly is the _configuration_ here?  One could argue that both could be
considered input, or that both could be considered configuration.  Our rule of
the thumb in porcupine is that everything than can be given a default value and
a help string should be displayed in the yaml configuration file (like the
`users` field in example1), and should generate a CLI flag to be able to alter
it on the fly. Everything else should just be looked in external files. _But
these are just the default behaviours._

Our goal is that there should be the fewest possible differences between a
pipeline's inputs and its configuration. Every call to `getOption(s)` will
internally declare a VirtualFile, and is only a thin layer on top of
`loadData`. That means that a record of options passed to `getOptions` will be
treated like any other input, and can perfectly be moved to a dedicated JSON or
YAML file instead of being stored in the `data:` section of the pipeline
configuration file.

Conversely, any `VirtualFile` that uses the JSONSerial can be directly embedded
in the `data:` section. Don't forget in that case to map that VirtualFile to
`null` in the `locations:` section, or else the pipeline will try to access a
file that doesn't exist.

## Location layers

Every VirtualFile (be it read from embedded data of from external files) can be
read from _several_ sources instead of one. That requires one thing though: that
the type `B` you read from a `VirtualFile A B` is an instance of Semigroup (call
`usesLayeredMapping` so your VirtualFile registers that). This way, porcupine
has a way to merge all these inputs into just one resource. It can come very
handy, and can be used depending on your pipeline to organize your input
resources into several layers, "stacked" so that the first layers' content are
completed or overriden by the subsequent layers. It all depends on which
instance of Semigroup is read from your `VirtualFile`.

In a similar fashion, if `B` is a Monoid (and if `canBeUnmapped` is applied to
your `VirtualFile`), this authorizes you to map it to `null` in your config
file, so when this `VirtualFile` is "read", we just get a `mempty`.

Layers also work for output files, although it's much simpler here: the data is
just identically written to every layer. No requirements are therefore put on
the type being written.

## Location accessors

For now, the examples we saw only deal with local resources. Porcupine features
an extensible API to declare other possible sources, via the `LocationAccessor`
class. Each location accessor just needs to be imported and declared at the time
of `runPipelineTask`, and that's all. Just do it and your whole pipeline's
resources can now be mapped to a new source of data. A very common source of
data is HTTP, and the `LocationAccessor` for it is provided in
`porcupine-http`. Support for Amazon S3 is also provided in `porcupine-s3`.

The example
[example-pokeapi](porcupine-http/examples/example-Poke/ExamplePokeAPI.hs) shows
how to deal with some data from the [PokeAPI](https://pokeapi.co). You can see
that the code of the pipeline is very much like that of `example1`, the major
difference being the call to `runPipelineTask`:

```haskell
main = runPipelineTask (FullConfig .......)
                       (  #http <-- useHTTP
                       :& baseContexts "")
                       mainTask ()
```

We explicitly state that we want to use the `#http` LocationAccessor (and that
we want to run it with the `useHTTP` function, imported from
`Data.Locations.Accessors.HTTP`) on top of the `baseContexts` (which provide
access to local files and logging). Note that you need to activate the
`OverloadedLabels` GHC extension.

Note that _nothing_ in the code tells us where the data will actually be read
from. The connection between our dataSource and the REST API will be made in the
[configuration file](porcupine-http/examples/example-Poke/example-pokeapi.yaml).

## Logging

`porcupine` uses `katip` to do logging. It's quite a versatile tool, and we
benefit from it. By default, logging uses a custom readable format. You can
switch to more standard formats using:

```sh
$ my-exe --log-format json
```

In any case, any executable using `porcupine` will output a log whose verbosity
can be controlled with `-q`/`-v` (increase/decrease the minimal severity level
that will be displayed, default being Notice). The `-c` (or `--context-verb`)
parameter controls the amount of context that will be displayed per item logged,
it takes a parameter from 0 to 3.

# Other resources

- [Introduction to porcupine @Haskell Exchange, in London, October 11th, 2019](https://skillsmatter.com/skillscasts/14236-porcupine-flows-your-rows-with-arrows)
