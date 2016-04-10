module Sarsi.Trace where

import Data.Machine ((<~), asParts, auto, runT_)
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text.Machine (streamParser)
import System.IO (Handle)
import System.IO.Machine (byLine, printer, sourceHandle)

import qualified Codec.GHC.Log as GHC
import qualified Codec.Sarsi.SBT as SBT
import qualified Data.Text as Text

traceHS :: Handle -> IO ()
traceHS = traceParser GHC.messageParser

traceSBT :: Handle -> IO ()
traceSBT = traceParser SBT.eventParser

{--
cleanEC = asParts <~ auto unpack <~ streamParser SBT.cleanEC
  where
    unpack (Right txt)  = [txt]
    unpack (Left _)     = []
    --}

traceParser :: Show a => Parser a -> Handle -> IO ()
traceParser parser h = do
  runT_ $ printer <~ {-- streamParser parser <~ cleanEC <~ --} appendCR <~ sourceHandle byLine h
    where
      appendCR = auto $ (`Text.snoc` '\n')


