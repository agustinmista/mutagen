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
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (takeExtension, (<.>), (</>))

{-------------------------------------------------------------------------------
-- * Trace metadata
-------------------------------------------------------------------------------}

-- | Folder where to put the generated metadata
tracerMetadataDir :: FilePath
tracerMetadataDir = ".mutagen"

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
saveTracerMetadata :: TracerMetadata -> FilePath -> IO ()
saveTracerMetadata metadata dir = do
  createDirectoryIfMissing True dir
  forM_ (Map.toList (tracerModules metadata)) $ \(modName, modMetadata) ->
    encodeFile (dir </> modName <.> "json") modMetadata

-- | Load the generated instrumentation metadata
loadTracerMetadata :: FilePath -> IO TracerMetadata
loadTracerMetadata dir = do
  let isJSON file = takeExtension file == ".json"
  files <- filter isJSON <$> listDirectory dir
  modules <- forM files $ \file -> do
    eitherDecodeFileStrict (dir </> file) >>= \case
      Left err -> throwIO $ MutagenInstantiationMetadataError err
      Right modMetadata -> return (file, modMetadata)
  return $ TracerMetadata (Map.fromList modules)

-- | Exception thrown loading instantiation metadata
data MutagenInstantiationMetadataError
  = MutagenInstantiationMetadataError String
  deriving (Show)

instance Exception MutagenInstantiationMetadataError

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
