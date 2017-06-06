# HaskellBF
A BrainFuck Interpreter in Haskell

## Usage

1. `git clone https://github.com/Agnishom/HaskellBF.git`
2. `cd haskellBF`
3. `ghc brainFuck.hs`
4. `cat program.bf | ./brainFuck`

## Example

```bash
[agnishom@agnishom-pc haskellCode]$ ./brainFuck 
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.Hello World!
[agnishom@agnishom-pc haskellCode]$ ghc brainFuck.hs 
[1 of 1] Compiling Main             ( brainFuck.hs, brainFuck.o )
Linking brainFuck ...
[agnishom@agnishom-pc haskellCode]$ ./brainFuck 
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
Hello World!
```
