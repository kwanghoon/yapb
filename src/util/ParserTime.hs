module ParserTime(startTime,finishTime,toSecond) where

import CommonParserUtil

import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans(lift)
import System.CPUTime
import Text.Printf
import GHC.IO.Handle.FD(stderr)

startTime :: ST.StateT (LexerParserState a) IO Integer
startTime = lift getCPUTime

finishTime :: Integer -> ST.StateT (LexerParserState a) IO ()
finishTime sTime = 
  do fTime <- lift getCPUTime
     -- lift ( putStrLn $ "parse time: start time: " ++ show sTime )
     -- lift ( putStrLn $ "parse time: finish time: " ++ show fTime )
     lift ( hPrintf stderr "parse time: %6.2fs\n" (toSecond (fTime - sTime)) )

toSecond :: Integer -> Float
toSecond cpuTime = fromIntegral cpuTime * 1e-12
