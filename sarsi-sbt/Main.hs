module Main where

import Codec.Sarsi.SBT (byLine, cleanEC, eventParser)
import Data.Attoparsec.Text.Machine (streamParser)
import Data.Machine (ProcessT, (<~), asParts, auto, autoM, runT_)
import System.Environment (getArgs)
import System.Exit (ExitCode)
import System.Process (CreateProcess, StdStream(..), shell, std_in, std_out, std_err)
import System.Process.Machine (callProcessMachines, mStdOut)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin, stdout)
import System.IO.Machine (byChunk, printer)

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Sarsi as Sarsi

title :: String
title = concat [Sarsi.title, "-sbt"]

-- callShell :: String -> IO (ExitCode, [a])
callShell cmd = callProcessMachines byChunk createProc (mStdOut pipeline)
  where
    -- pipeline = printer <~ streamParser eventParser <~ asParts <~ auto unpack <~ streamParser cleanEC <~ echoText stdout
    pipeline = printer <~ preprocessing <~ echoText stdout
    echoText h = autoM $ (\txt -> TextIO.hPutStr h txt >> return txt)
    createProc  = (shell cmd) { std_in = Inherit, std_out = CreatePipe }
    --preprocessing = asParts <~ auto unpackEC <~ streamParser cleanEC <~ asParts <~ auto unpackLine <~ streamParser byLine
    preprocessing = asParts <~ auto unpackLine <~ streamParser byLine
      where
        unpackEC (Right txt)  = [txt]
        unpackEC (Left _)     = []
        unpackLine (Right txt)  = [txt `Text.snoc` '\n']
        unpackLine (Left _)     = []

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args      <- getArgs
  (ec, _) <- callShell $ concat $ List.intersperse " " ("sbt":args)
  putStrLn $ concat [title, ": ", show ec]

-- producer (concat $ List.intersperse " " args)
