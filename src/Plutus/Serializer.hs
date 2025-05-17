{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Plutus.Serializer
  ( -- * Core functionality
    writePlutusScript,
    writeMyValidatorScript
  ) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api as Plutus
import Cardano.Api.Shelley (PlutusScript (..))
import Prelude (FilePath, IO)

import Contract (myValidator)
import PlutusTx
import PlutusTx.Prelude

-- | Compile the Plutus validator
validator :: Plutus.Validator
validator = mkValidatorScript $$(PlutusTx.compile [||myValidator||])

-- | Write a Plutus Script to a file
writePlutusScript :: FilePath -> Plutus.Validator -> IO (Either (FileError ()) ())
writePlutusScript file =
  writeFileTextEnvelope @(PlutusScript PlutusScriptV2)
    file
    Nothing
    . PlutusScriptSerialised
    . SBS.toShort
    . LBS.toStrict
    . serialise
    . Plutus.unValidatorScript

-- | Generate and write `myValidator` to a file
writeMyValidatorScript :: IO (Either (FileError ()) ())
writeMyValidatorScript = writePlutusScript "myValidator.plutus" validator
