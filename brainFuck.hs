import Data.Either
import Data.List
import Data.Char
import System.Environment
import System.IO
import Control.Monad

type Program = [Instruction]

data Instruction = 
              Lft
            | Rgt
            | Inc
            | Dec
            | Nop
            | Loop Program
            | Out
            | Inp
            deriving (Show, Eq)

data Memory = Memory [Int] [Int]
   deriving Show

emptyMem :: Memory
emptyMem = Memory [] [0]

parse :: String -> Program
parse xs = go xs []
   where
      go :: String -> [Either Char Instruction] -> Program
      go "" is      = reverse $ rights is
      go ('[':xs) is   = go xs ((Left '['):is)
      go (']':xs) is   = go xs ((Right l):l2)
         where
            (l1, (_:l2)) =  span (/= Left '[') is
            l = Loop $ (rights.reverse) l1
      go ('>':xs) is   = go xs (Right Rgt:is)
      go ('<':xs) is   = go xs (Right Lft:is)
      go ('+':xs) is   = go xs (Right Inc:is)
      go ('-':xs) is   = go xs (Right Dec:is)
      go ('.':xs) is   = go xs (Right Out:is)
      go (',':xs) is   = go xs (Right Inp:is)

runInstruction :: Instruction -> Memory -> IO Memory
runInstruction Nop mem  = pure mem
-- moving the location of the head
runInstruction Rgt (Memory xs (y:[])) = pure (Memory (y:xs) [0])
runInstruction Rgt (Memory xs (y:ys)) = pure (Memory (y:xs) ys)
runInstruction Lft (Memory (x:xs) ys) = pure (Memory xs (x:ys))
runInstruction Lft (Memory [] ys)     = pure (Memory [] (0:ys))
-- loop until the head reads 0
runInstruction (Loop _) mem@(Memory xs (0:ys))  = pure mem
runInstruction (Loop p) mem                     = runProgram p mem 
                                                  >>= runInstruction (Loop p)
-- increment/decrement the head
runInstruction Inc (Memory xs (y:ys)) = pure (Memory xs ((y+1):ys))
runInstruction Dec (Memory xs (y:ys)) = pure (Memory xs ((y-1):ys))
-- output the head
runInstruction Out mem@(Memory _ (x:xs)) = putChar (chr x) >> pure mem
-- get input and store it in the head
runInstruction Inp(Memory xs (y:ys))     = getChar >>= \z -> pure (Memory xs ((ord z):ys))

runProgram :: Program -> Memory -> IO Memory
runProgram is = foldr (>=>) pure (runInstruction <$> is) 

main :: IO ()
main = do
   (fileName:_) <- getArgs
   prog <- (parse . (filter (`elem` "[]+-><.,"))) <$> readFile fileName
   void $ runProgram prog emptyMem
