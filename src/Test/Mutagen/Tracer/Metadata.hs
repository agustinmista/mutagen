{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

-- | Tracing metadata
--
-- Mutagen saves some metadata about the instrumentation it performs at compile
-- time, such as the number of tracing nodes generated, along with their
-- original source location and kind.
--
-- This information is later used to efficiently allocate enough memory for the
-- tracing data structures at runtime, as well as mapping runtime coverage data
-- back to source locations for reporting.
module Test.Mutagen.Tracer.Metadata
  ( -- * Trace metadata
    tracerMetadataDir
  , TracerMetadata (..)
  , mkSingleModuleTracerMetadata
  , numTracingNodes
  , saveTracerMetadata
  , loadTracerMetadata

    -- * Module metadata
  , ModuleMetadata (..)

    -- * Node metadata
  , NodeType (..)
  , NodeLocation (..)
  , NodeMetadata (..)
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM, forM_)
import Control.Monad.Extra (whenM)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , listDirectory
  , removeDirectoryRecursive
  )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
-- * Trace metadata
-------------------------------------------------------------------------------}

-- | Folder where to put the generated metadata
--
-- NOTE: can be overridden via the 'MUTAGEN_TRACER_METADATA_DIR' environment
-- variable. Just remember to have it set up to the same value both at compile
-- time and at runtime.
tracerMetadataDir :: FilePath
tracerMetadataDir =
  fromMaybe ".mutagen"
    $ unsafePerformIO
    $ lookupEnv "MUTAGEN_TRACER_METADATA_DIR"
{-# NOINLINE tracerMetadataDir #-}

-- | Generated instrumentation metadata
newtype TracerMetadata = TracerMetadata
  { tracerModules :: Map FilePath ModuleMetadata
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON TracerMetadata

instance FromJSON TracerMetadata

-- | Create tracer metadata for a single module
mkSingleModuleTracerMetadata :: FilePath -> ModuleMetadata -> TracerMetadata
mkSingleModuleTracerMetadata modName metadata =
  TracerMetadata (Map.singleton modName metadata)

-- | Return the number of tracing nodes for an entire instrumentation run
numTracingNodes :: TracerMetadata -> Int
numTracingNodes metadata =
  length (concatMap moduleTracingNodes (tracerModules metadata))

-- | Save the generated instrumentation metadata
--
-- NOTE: to avoid loading potentially invalid metadata when a source file gets
-- deleted, this function recreates the target directory if it already exists.
saveTracerMetadata :: TracerMetadata -> IO ()
saveTracerMetadata metadata = do
  whenM (doesDirectoryExist tracerMetadataDir) $ do
    removeDirectoryRecursive tracerMetadataDir
  createDirectoryIfMissing True tracerMetadataDir
  forM_ (Map.toList (tracerModules metadata)) $ \(modName, modMetadata) ->
    encodeFile (tracerMetadataDir </> modName <.> "json") modMetadata

-- | Load the generated instrumentation metadata
loadTracerMetadata :: IO TracerMetadata
loadTracerMetadata = do
  whenM (not <$> doesDirectoryExist tracerMetadataDir) $ do
    throwIO
      $ MutagenTracerMetadataError
      $ "Tracer metadata directory does not exist: " <> tracerMetadataDir
  files <- filter isJSON <$> listDirectory tracerMetadataDir
  modules <- forM files $ \file -> do
    eitherDecodeFileStrict (tracerMetadataDir </> file) >>= \case
      Left err -> throwIO $ MutagenTracerMetadataError err
      Right modMetadata -> return (file, modMetadata)
  return $ TracerMetadata (Map.fromList modules)
  where
    isJSON file = takeExtension file == ".json"

-- | Exception thrown loading instantiation metadata
data MutagenTracerMetadataError
  = MutagenTracerMetadataError String
  | MutagenTracerMetadataIOError String
  deriving (Show)

instance Exception MutagenTracerMetadataError

{-------------------------------------------------------------------------------
-- * Module metadata
-------------------------------------------------------------------------------}

-- | Tracer metadata of a single module
newtype ModuleMetadata = ModuleMetadata
  { moduleTracingNodes :: [NodeMetadata]
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON ModuleMetadata

instance FromJSON ModuleMetadata

{-------------------------------------------------------------------------------
-- * Node metadata
-------------------------------------------------------------------------------}

-- | Kind of tracing node
data NodeType = GRHSNode | ThenNode | ElseNode
  deriving (Eq, Generic, Read, Show)

instance ToJSON NodeType

instance FromJSON NodeType

-- | Node locations
data NodeLocation = NodeLocation
  { filePath :: FilePath
  , startLine :: Int
  , startCol :: Int
  , endLine :: Int
  , endCol :: Int
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON NodeLocation

instance FromJSON NodeLocation

-- | Tracer metadata of a single tracing node
data NodeMetadata = NodeMetadata
  { nodeId :: Int
  , nodeKind :: NodeType
  , nodeLocation :: Maybe NodeLocation
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON NodeMetadata

instance FromJSON NodeMetadata
