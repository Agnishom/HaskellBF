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

runInstruction :: Memory -> Instruction -> IO (IO Memory, IO ())
runInstruction mem Nop = return (return mem, return ())
runInstruction (Memory xs (y:[])) Rgt = return (return (Memory (y:xs) [0]), return ())
runInstruction (Memory xs (y:ys)) Rgt = return (return (Memory (y:xs) ys), return ())
runInstruction (Memory (x:xs) ys) Lft = return (return (Memory xs (x:ys)), return ())
runInstruction (Memory [] ys) Lft = return (return (Memory [] (0:ys)), return ())
runInstruction (Memory xs (y:ys)) Inc = return (return (Memory xs ((y+1):ys)), return ())
runInstruction (Memory xs (y:ys)) Dec = return (return (Memory xs ((y-1):ys)), return ())
runInstruction mem@(Memory _ (x:xs)) Out = return (return mem, putChar (chr x))
runInstruction (Memory xs (y:ys)) Inp = return (mem', return ())
                                          where mem' = getChar >>= \z -> return (Memory xs ((ord z):ys))
runInstruction mem@(Memory xs (0:ys)) (Loop _) = return (return mem, return ())
runInstruction mem (Loop prog) = do
                                    (mem1, act') <- runProgram mem prog
                                    mem' <- mem1
                                    (mem'', act'') <- runInstruction mem' (Loop prog)
                                    return (mem'', act' >> act'')



runProgram :: Memory -> Program -> IO (IO Memory, IO ())
runProgram mem [] = return (return mem, return ())
runProgram mem (i:is) = do
                           (mem1, act1) <- runInstruction mem i
                           mem' <- mem1
                           (mem2, act2) <- runProgram mem' is
                           return (mem2, act1 >> act2)

main :: IO ()
main = do
   prog <- getContents
   (mem, act) <- runProgram emptyMem (parse (filter (`elem` "[]+-><.,") prog))
   act
