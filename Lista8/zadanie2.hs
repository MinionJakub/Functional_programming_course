module ST where

import Data.Char as DChar
import System.IO (isEOF)
import Control.Monad


-- data StreamTrans i o a
--     = Return a
--     | ReadS (Maybe i -> StreamTrans i o a)
--     | WriteS o (StreamTrans i o a)

-- my_toLower :: StreamTrans Char Char ()
-- my_toLower = ReadS (\ input -> 
--     case input of
--     Nothing -> Return ()
--     Just ch -> WriteS (DChar.toLower ch) my_toLower)

-- import Data.Char (toLower)
-- -- import Data.Char (EOF)
-- import System.IO (isEOF)  
-- -- jak to testować...
data StreamTrans input output ans = Return ans 
  | ReadS (Maybe input -> StreamTrans input output ans) 
  | WriteS output (StreamTrans input output ans)

-- -- data Tree a = Branch (Tree a) (Tree a) | Leaf a

my_toLower :: StreamTrans Char Char ans 
my_toLower = ReadS(\x -> case x of 
  Nothing -> my_toLower
  Just x -> WriteS (toLower x) my_toLower)

-- my_toLower = ReadS (\ input -> 
--     case input of
--     Nothing -> Return ()
--     Just ch -> WriteS (toLower ch) my_toLower)


runIOStreamTrans :: StreamTrans Char Char ans -> IO ans
runIOStreamTrans (Return ans) = do return ans
runIOStreamTrans (ReadS func) = do
  end <- isEOF
  if end
    then runIOStreamTrans (func Nothing)
    else do 
      x <- getChar
      runIOStreamTrans $ func (Just x)
runIOStreamTrans (WriteS output stream) = do
  putChar output 
  runIOStreamTrans stream

--zadanie 3
listTrans :: StreamTrans input output ans -> [input] -> ([output],ans)
listTrans stream lst =
    _listTrans stream lst []
    where _listTrans st xs acc = case st of {
            Return a  -> ((reverse acc),a);
            ReadS cps -> case xs of {
                []   -> _listTrans (cps Nothing) xs acc;
                h:tl -> _listTrans (cps (Just h)) tl acc};
            WriteS o cps -> _listTrans cps xs (o : acc)}

--zadanie 4

runCycle :: StreamTrans a a b -> b
runCycle (Return ans) = ans
runCycle (WriteS output (Return ans)) = ans
runCycle (WriteS output (WriteS output2 stream)) = runCycle (WriteS output2 stream)
runCycle (WriteS output (ReadS func)) = runCycle (func (Just output))
runCycle (ReadS func) = runCycle $ func Nothing

--zadanie 5
(|>|) :: StreamTrans i m a -> StreamTrans m o b -> StreamTrans i o b
(|>|) _ (Return b) = (Return b)
(|>|) (Return a) (ReadS k) = (Return a) |>| (k Nothing)
(|>|) (ReadS c) (ReadS k) = ReadS (\ input -> (c input) |>| (ReadS k))
(|>|) (WriteS o c) (ReadS k) = c |>| (k (Just o))
(|>|) st (WriteS o k) = WriteS o (st |>| k)

--zadanie 6
catchOutput :: StreamTrans i o a -> StreamTrans i b (a, [o])
catchOutput stream =
    _captureOutput stream []
    where _captureOutput st acc = case st of {
        Return a  -> Return (a,(reverse acc));
        ReadS cps -> ReadS (\ input -> _captureOutput (cps input) acc);
        WriteS o cps -> _captureOutput cps (o : acc)}

