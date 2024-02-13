import System.IO(isEOF)
import Data.Char(toLower)

echoLower::IO()

echoLower = do
  end <- isEOF
  if end then return ()
  else do 
    x <- getLine
    (mapM (putChar . toLower)) x
    putChar('\n')
    echoLower