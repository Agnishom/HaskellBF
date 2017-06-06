import Data.Either
import Data.List
import Data.Char

type Program = [Instruction]

data Instruction = Lft
            | Rgt
            | Inc
            | Dec
            | Nop
            | Loop Program
            | Out
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

runInstruction :: Memory -> Instruction -> (Memory, IO ())
runInstruction mem Nop = (mem, return ())
runInstruction mem@(Memory _ (x:xs)) Out = (mem, putChar (chr x))
runInstruction (Memory xs (y:[])) Rgt = ((Memory (y:xs) [0]), return ())
runInstruction (Memory xs (y:ys)) Rgt = ((Memory (y:xs) ys), return ())
runInstruction (Memory (x:xs) ys) Lft = ((Memory xs (x:ys)), return ())
runInstruction (Memory [] ys) Lft = ((Memory [] (0:ys)), return ())
runInstruction (Memory xs (y:ys)) Inc = ((Memory xs ((y+1):ys)), return ())
runInstruction (Memory xs (y:ys)) Dec = ((Memory xs ((y-1):ys)), return ())
runInstruction mem@(Memory xs (0:ys)) (Loop _) = (mem, return ())
runInstruction mem (Loop prog) = let (mem', act') = runProgram mem prog in 
                              let (mem'', act'') = runInstruction mem' (Loop prog) in
                                 (mem'', act' >> act'')

runProgram :: Memory -> Program -> (Memory, IO ())
runProgram mem [] = (mem, return ())
runProgram mem (i:is) = let (mem', act') = runInstruction mem i in
                     let (mem'', act'') = runProgram mem' is in
                        (mem'', act' >> act'')

main :: IO ()
main = do
   prog <- getContents
   snd $ runProgram emptyMem (parse (filter (`elem` "[]+-><.") prog))
