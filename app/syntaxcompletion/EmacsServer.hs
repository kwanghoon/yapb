module EmacsServer where

import SynCompInterface
  
import Network.Socket hiding (recv,send)
import Network.Socket.ByteString
import Data.ByteString.Char8
import Control.Monad
import Control.Exception

type ComputeCandidate = String -> Int -> IO [EmacsDataItem]

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
    cursorPos <- getCursorPos conn
    print cursorPos
    (conn, _) <- accept sock
    str <- getSource conn
    print str
    candidateList <- computeCand str cursorPos
    print (Prelude.map show candidateList)
    (conn, _) <- accept sock
    sendCandidateList conn candidateList
    close conn

str2int :: String -> Int
str2int str = read str :: Int

getCursorPos :: Socket -> IO Int
getCursorPos conn = do
    str <- recv conn 64
    return (str2int (unpack str))

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
