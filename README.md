# HaskellBF
A BrainFuck Interpreter in Haskell

## Usage

1. `git clone https://github.com/Agnishom/HaskellBF.git`
2. `cd haskellBF`
3. `ghc brainFuck.hs`
4. `./brainFuck hello.bf`

## Example

```bash
[agnishom@agnishom-pc HaskellBF]$ cat hello.bf 
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
[agnishom@agnishom-pc HaskellBF]$ ghc brainFuck.hs 
[1 of 1] Compiling Main             ( brainFuck.hs, brainFuck.o )
Linking brainFuck ...
[agnishom@agnishom-pc HaskellBF]$ ./brainFuck hello.bf
Hello World!
```
