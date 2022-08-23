-- I Make This Version Is Pure Roulete game

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week04.PlutusRoletOnly where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

import Week04.RoletPureLogic

import Plutus.Trace
import Wallet.Emulator.Wallet
import           Plutus.Trace.Emulator  as Emulator
import Plutus.Contract.Test
import Ledger.TimeSlot




data TheDatum = TheDatum
    { owner    :: PaymentPubKeyHash
    , amount      :: Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''TheDatum

{-# INLINABLE mkValidator #-}
mkValidator :: TheDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "It not False, cause it already True on Off-Chain. If amout not 0" isthatTrue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    isthatTrue :: Bool
    isthatTrue = (amount dat) > 0

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = TheDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @TheDatum @()


-------- Off-Chain code -----------------------------------------


validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpOwner    :: !PaymentPubKeyHash
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "lock" Integer
        .\/ Endpoint "bet" BetParams


lock :: AsContractError e => Integer -> Contract w s e ()
lock gp = do
    pkh   <- ownPaymentPubKeyHash
    let dat = TheDatum
                { owner = pkh
                , amount  = gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx


data BetParams = BetParams
    { yourBetValue :: !String
    , amountt   :: !Integer
    }
    deriving (Generic, ToJSON, FromJSON, ToSchema)



bet :: forall w s e. AsContractError e => BetParams -> Contract w s e ()
bet (BetParams theBet a) = do
    pkh   <- ownPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    let has = theBet
        bolhasilbet = mencariHasil has
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            if bolhasilbet
                then do
                    let list = Map.toList utxos
                        oref = head $ fst <$> list
                        utxo = head list
                        lookups = Constraints.unspentOutputs utxos <> 
                                Constraints.otherScript validator <>
                                Constraints.typedValidatorLookups typedValidator
                        d = getDatum $ snd utxo
                        remainder = amount d - a
                        dat = TheDatum 
                                { owner    = owner d
                                , amount      = remainder
                                }
                        tx = Constraints.mustPayToTheScript dat (Ada.lovelaceValueOf remainder) <>
                             Constraints.mustPayToPubKey pkh (Ada.lovelaceValueOf a) <>
                             Constraints.mustSpendScriptOutput oref unitRedeemer
                    ledgerTx <- submitTxConstraintsWith @Vesting lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo $ "collected gift amount:" <> show a
                else do
                    let list = Map.toList utxos
                        oref = head $ fst <$> list
                        utxo = head list
                        lookups = Constraints.unspentOutputs utxos <> 
                                Constraints.otherScript validator <>
                                Constraints.typedValidatorLookups typedValidator
                        d = getDatum $ snd utxo
                        remainder = amount d + a
                        dat = TheDatum 
                                { owner    = owner d
                                , amount      = remainder
                                }
                        tx = Constraints.mustPayToTheScript dat (Ada.lovelaceValueOf remainder) <>
                             Constraints.mustSpendScriptOutput oref unitRedeemer
                    ledgerTx <- submitTxConstraintsWith @Vesting lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo $ "collected gift amount:" <> show a

  where
    getDatum :: ChainIndexTxOut -> TheDatum
    getDatum o = case _ciTxOutDatum o of
            Left  _ -> traceError "Datum does not exist"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> traceError "Unknown datum type"
                Just d  -> d




boolFromIO :: IO Bool -> Bool
boolFromIO = boolFromIO

mencariHasil:: String-> Bool
mencariHasil a = boolFromIO (inputBetValue a)



endpoints ::   Contract () VestingSchema Text ()
endpoints = awaitPromise (lock' `select` bet') >> endpoints
  where
    lock' = endpoint @"lock" lock
    bet' = endpoint @"bet" bet


-- mkSchemaDefinitions ''VestingSchema

-- mkKnownCurrencies []



test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    let w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints 
    h2 <- activateContractWallet w2 endpoints

    callEndpoint @"lock" h1 $ 90000000
      
    void $ Emulator.waitNSlots 5

    callEndpoint @"bet" h2 $ BetParams
      { yourBetValue = "2"
      , amountt   = 1000000
      }
    void $ Emulator.waitNSlots 5

    callEndpoint @"bet" h2 $ BetParams
      { yourBetValue = "2"
      , amountt   = 1000000
      }
    void $ Emulator.waitNSlots 5

