module EmacsServer where

import SynCompInterface
  
import Network.Socket hiding (recv,send)
import Network.Socket.ByteString
import Data.ByteString.Char8
import Control.Monad
import Control.Exception

type ComputeCandidate = String -> Bool -> Int -> IO [EmacsDataItem]

emacsServer :: ComputeCandidate -> IO ()
emacsServer f = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 50000 0)
    listen sock 5
    acceptLoop f sock `finally` close sock

acceptLoop :: ComputeCandidate -> Socket -> IO ()
acceptLoop computeCand sock = forever $ do
    (conn, _) <- accept sock
    (cursorPos, isSimple) <- getCursorPos_and_isSimple conn
    print (cursorPos, isSimple)
    (conn, _) <- accept sock
    str <- getSource conn
    print str
    candidateList <- computeCand str isSimple cursorPos
    print (Prelude.map show candidateList)
    (conn, _) <- accept sock
    sendCandidateList conn candidateList
    close conn

str2cursorPos_and_isSimple :: String -> (Int,Bool)
str2cursorPos_and_isSimple str =
  let [s1,s2] = Prelude.words str
  in (read s1 :: Int, read s2 :: Bool)

getCursorPos_and_isSimple :: Socket -> IO (Int, Bool)
getCursorPos_and_isSimple conn = do
    str <- recv conn 64
    return (str2cursorPos_and_isSimple (unpack str))

getSource :: Socket -> IO String
getSource conn = do
    str <- recv conn 64
    if Data.ByteString.Char8.length str == 0 then
      return (unpack str)
    else do
      aaa <- getSource conn
      return ((unpack str) ++ aaa)

sendCandidateList :: Socket -> [EmacsDataItem] -> IO ()
sendCandidateList conn xs = do
    let
      f [] = ""
      f ((Candidate x) : xs)      = "\n" ++ x ++ f xs
      f (LexError : xs)           = "LexError" ++ f xs
      f ((ParseError _) : xs)     = "ParseError" ++ f xs
      f (SuccessfullyParsed : xs) = "SuccessfullyParsed" ++ f xs
    let
      s = f xs
    do
      _ <- send conn (pack s)
      print s
