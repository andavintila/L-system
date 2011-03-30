import System.Environment
main = do
		args <- getArgs
		x <- readFile "strings.out"
		y <- readFile (head args)
		let instr = varInstr (map head (split (lines y))) (instruct (lines y))
		writeFile "prog.turtle" (concat ("variables\n" : "endvar\n\n" : (map (replace instr) x) ++ ["done"]))

split a
	| null a	= []
	| otherwise	= head (words (head a)):split (tail a)

instruct a
	| null (head a)					= []
	| length (words (head a)) == 3 	= (drop 2 (words (head a))) ++ instruct (tail a)
	| otherwise						= unwords (drop 2 (words (head a))) : instruct (tail a)
	
varInstr a b
	| null b	= []
	| otherwise	= (head a,head b):varInstr (tail a) (tail b)

findInstr a (b,c) = (a == b)

replace a b  = snd( head( filter (findInstr b)  a )) ++ "\n"
		