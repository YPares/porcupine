{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -Wall          #-}

module System.TaskPipeline.CLI.Overriding
  ( CLIOverriding(..)
  , docRecBasedCLIOverriding
  , genericAesonBasedCLIOverriding
  , parseJSONEither
  , addScribeParamsParsing
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson              as A
import           Data.DocRecord
import           Data.DocRecord.OptParse
import qualified Data.HashMap.Lazy       as HashMap
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Yaml               as Y
import           Options.Applicative
import           System.Logger           (LoggerScribeParams (..),
                                          Severity (..), Verbosity (..))

-- | How to override a YAML file config from the command-line
data CLIOverriding cfg overrides = CLIOverriding
  { overridesParser    :: Parser overrides
  -- ^ Generate a parser from default cfg
  , nullOverrides      :: overrides -> Bool
  -- ^ True if no override has been provided on the CLI
  , overrideCfgFromYamlFile
      :: A.Value -> overrides -> ([String], Either String cfg)
  -- ^ How to override the config read from YAML file. Returns: (Warnings,
  -- Overriden config or an error).
  }

-- | Parses the CLI options that will be given to Katip's logger scribe
parseScribeParams :: Parser LoggerScribeParams
parseScribeParams = LoggerScribeParams
  <$> ((option (eitherReader severityParser)
          (long "severity"
      ))
      <|>
      (numToSeverity . length <$>
        (many
          (flag' ()
            (  short 'q'
            <> help "Don't print configuration (-q) and warnings (-qq)")))))
  <*> (numToVerbosity . length <$>
        (many
          (flag' ()
            (  long "verbose"
            <> short 'v'
            <> help "Controls the amount of information to display for each logged message"))))
  where
    numToSeverity 0 = InfoS
    numToSeverity 1 = ErrorS
    numToSeverity _ = EmergencyS -- by convention for no output
    severityParser = \case
        "debug" -> Right DebugS
        "info" -> Right InfoS
        "warning" -> Right WarningS
        "error" -> Right ErrorS
        "critical" -> Right CriticalS
        "none" -> Right EmergencyS -- by convention for no output
        s -> Left $ s ++ " isn't a valid severity level"
    numToVerbosity 0 = V0
    numToVerbosity 1 = V0
    numToVerbosity 2 = V2
    numToVerbosity _ = V3

-- | Modifies a CLI parsing so it features verbosity and severity flags
addScribeParamsParsing :: CLIOverriding cfg ovs -> CLIOverriding (LoggerScribeParams, cfg) (LoggerScribeParams, ovs)
addScribeParamsParsing super = CLIOverriding
  { overridesParser = (,) <$> parseScribeParams <*> overridesParser super
  , nullOverrides = \(_, ovs) -> nullOverrides super ovs
  , overrideCfgFromYamlFile = \yaml (scribeParams, ovs) ->
      let (warns, res) = overrideCfgFromYamlFile super yaml ovs
      in (warns, (scribeParams,) <$> res)
  }

parseJSONEither :: (A.FromJSON t) => A.Value -> Either String t
parseJSONEither x = case A.fromJSON x of
  A.Success s -> Right s
  A.Error r   -> Left r
{-# INLINE parseJSONEither #-}

-- | defCfg must be a 'DocRec' here. Uses it to generate one option per field in
-- the DocRec, along with its documentation.
docRecBasedCLIOverriding
  :: (RecordUsableWithCLI rs)
  => DocRec rs -> CLIOverriding (DocRec rs) (Rec SourcedDocField rs)
docRecBasedCLIOverriding defCfg = CLIOverriding{..}
  where
    overridesParser = parseRecFromCLI $ tagWithDefaultSource defCfg
        -- The parser will set the source to CLI for each modified
        -- field. Unmodified fields' source with remain Default
    nullOverrides :: Rec SourcedDocField rs -> Bool
    nullOverrides RNil = True
    nullOverrides _    = False
    overrideCfgFromYamlFile aesonCfg cliOverrides = ([], result)
      where
        result = do
          yamlCfg <- tagWithYamlSource <$> parseJSONEither aesonCfg
          return $ rmTags $ rzipWith chooseHighestPriority yamlCfg cliOverrides
                        -- CLI overrides YAML and YAML overrides Default. This
                        -- way, options from the CLI that have not been changed
                        -- from their Default value do not erase the value
                        -- specified in the JSON file.

-- | Generates a --override/-o CLI option that takes a "key=value" argument and
-- can be repeated. Also has a -q flag that activates quiet mode. Doesn't make
-- assumptions on the types of values, but doesn't do extra checks and doesn't
-- display any extra documentation.
genericAesonBasedCLIOverriding
  :: (A.FromJSON cfg)
  => String -> [(String, Char, String)] -> CLIOverriding cfg [String]
genericAesonBasedCLIOverriding configFile shortcuts =
  CLIOverriding genParser null
    (\origCfg overrides ->
       let (warnings, result) =
             overrideConfigFromKeyValues origCfg overrides
       in (warnings, join $ parseJSONEither <$> result))
  where
    genParser = foldr1 (liftA2 (++)) overrideArgs
    mkOption (l,s,h,f) = f <$>
      many (strOption
             ( long l <> short s <> metavar "yaml.path=YAML_VALUE" <> help h ))
    mkShortcut (l,s,p) =
      ( l,s,"A shortcut for `-o "<>p<>".yaml.path=YAML_VALUE'"
      , map ((p++".")++) )
    overrideArgs = map mkOption $
      ("override", 'o', "Override a field value in the " <> configFile <>
       " configuration.", id)
      : map mkShortcut shortcuts

overrideConfigFromKeyValues :: A.Value -> [String] -> ([String], Either String A.Value)
overrideConfigFromKeyValues origCfg overrides =
  case foldM parseAndOverride ([], origCfg) $ map T.pack overrides of
    Left s       -> ([], Left s)
    Right (w, c) -> (w, Right c)
  where
    badPath fullPath =
      Left $ "Path `" ++ fullPath ++ "' malformed."
    pathNotFound fullPath fields =
      Left $ "Path `" ++ fullPath
             ++ "' contains unknown nested field(s): " ++ show fields
    parseAndOverride (w, cfg) override = case T.splitOn "=" override of
      [path, val] -> case Y.decodeEither' $ T.encodeUtf8 val of
        Right jsonVal -> do
          (w', cfg') <- doOverride cfg (T.unpack path) (T.splitOn "." path) jsonVal
          return (w'++w, cfg')
        Left e -> Left $ "`" ++ T.unpack path ++ "': `"
                   ++ T.unpack val ++ "' is not valid yaml: got"
                   ++ show e
      _ -> badPath $ T.unpack override
    doOverride _ _ [] v = Right ([],v)
    doOverride (A.Object cfg) fullPath (k:ks) v =
      case HashMap.lookup k cfg of
        Just cfg' -> do
          (w, cfg'') <- doOverride cfg' fullPath ks v
          Right $ checkTypeAndInsert w fullPath cfg' k cfg'' cfg
        Nothing -> case ks of
          [] -> Right $
            ( ["`" ++ fullPath ++
               "': This field does not exist in config file, it will be added (but beware of typos!)"]
            , A.Object $ HashMap.insert k v cfg)
          k' -> pathNotFound fullPath k'
    doOverride _ fullPath k _ = pathNotFound fullPath k

jsonType :: A.Value -> String
jsonType a = case a of
  A.String _ -> "a string"
  A.Object _ -> "an object"
  A.Number _ -> "a number"
  A.Array _  -> "an array"
  A.Bool _   -> "a bool"
  A.Null     -> "a null"

checkTypeAndInsert :: [String]
                   -> String
                   -> A.Value
                   -> T.Text
                   -> A.Value
                   -> HashMap.HashMap T.Text A.Value
                   -> ([[Char]], A.Value)
checkTypeAndInsert w fullPath v' k v m =
  let i = A.Object $ HashMap.insert k v m
      t = jsonType v
      t' = jsonType v'
  in if t == t'
     then (w,i)
     else
       (["`" ++ fullPath ++ "': Overriding " ++ t'
        ++ " with " ++ t] ++ w
       , i)
