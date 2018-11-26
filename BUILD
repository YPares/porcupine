package(default_visibility = ["//visibility:public"])

load("//:simwork/simwork_binary.bzl", "haskell_benchmark", "jinko_haskell_library")
load(
    "@io_tweag_rules_haskell//haskell:haskell.bzl",
    "haskell_library",
)

alias(name = "pipeline-tools", actual = ":porcupine-core")

jinko_haskell_library(
    name = "docrecords",
    srcs = glob(["docrecords/src/**/*.hs"]),
    src_strip_prefix = "docrecords/src",
    deps =
        [
            "@hackage//:base",
            "@hackage//:aeson",
            "@hackage//:data-default",
            "@hackage//:lens",
            "@hackage//:optparse-applicative",
            "@hackage//:text",
            "@hackage//:unordered-containers",
            "@hackage//:vinyl",
            "@hackage//:yaml",
        ],
)

jinko_haskell_library(
    name = "porcupine-core",
    srcs = glob(["porcupine-core/src/**/*.hs"]),
    src_strip_prefix = "porcupine-core/src",
    deps =
        [
            "@hackage//:aeson",
            "@hackage//:aeson-pretty",
            "@hackage//:amazonka",
            "@hackage//:amazonka-core",
            "@hackage//:amazonka-s3",
            "@hackage//:base",
            "@hackage//:binary",
            "@hackage//:binary-orphans",
            "@hackage//:bytestring",
            "@hackage//:clock",
            "@hackage//:conduit",
            "@hackage//:conduit-extra",
            "@hackage//:containers",
            "@hackage//:contravariant", # Won't be necessary in GHC 8.6
            "@hackage//:data-default",
            "@hackage//:deepseq",
            "@hackage//:directory",
            "@hackage//:exceptions",
            "@hackage//:filepath",
            "@hackage//:funflow",
            "@hackage//:formatting",
	    "@hackage//:foldl",
            "@hackage//:hashable",
            "@hackage//:katip",
            "@hackage//:lens",
            "@hackage//:monad-control",
            "@hackage//:mtl",
            "@hackage//:optparse-applicative",
            "@hackage//:path",
            "@hackage//:profunctors",
            "@hackage//:resourcet",
            "@hackage//:retry",
            "@hackage//:store",
            "@hackage//:streaming",
            "@hackage//:streaming-bytestring",
            "@hackage//:streaming-conduit",
            "@hackage//:template-haskell",
            "@hackage//:temporary",
            "@hackage//:text",
            "@hackage//:transformers",
            "@hackage//:transformers-base",
            "@hackage//:unliftio-core",
            "@hackage//:unordered-containers",
            "@hackage//:url",
            "@hackage//:void",
            "@hackage//:yaml",
            ":docrecords",
        ],
)
