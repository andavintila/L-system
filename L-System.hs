import System.Environment
main = do
		args <- getArgs
		x <- readFile (head args) --args- lista
		let n = read (head (tail (words (head (drop 3 (lines x))))))  --n = adancimea (lines x ) - ia toate liniile din fisier
		let reguli = varRule (map head (split (drop 5 (lines x)))) (rules (drop 5 (lines x))) ++ ct (tail (words (head (tail (lines x)))))
		let seed = head (tail (words (head (drop 2 (lines x))))) --retine var de start
		writeFile "strings.out" (head (drop (n-1) (take n (fluxl reguli seed)))) 
		
-- ia o lista cu linii(reguli) si returneaza o lista de stringuri (variabile)
split a
	| null a	= []
	| otherwise	= head (words (head a)):split (tail a)

--reguli indentitate pentru constante
ct [] = []
ct x = varRule (map head x) x

--ia regulile si le pune in lista (doar regula de dupa =)
rules a
	| null a	= []
	| otherwise	= head (tail (tail(words (head a)))):rules (tail a)

--face pereche variabila-regula	
varRule a b
	| null b	= []
	| otherwise	= (head a,head b):varRule (tail a) (tail b)

--gaseste regula pentru variabila
findRule a (b,c) = (a == b)

--regula cu care se inloc
find a b  = snd(head(filter(findRule b)a))

--inlocuieste varibila cu regula
replace a b = concat(map(find a) b)

--flux sistem L
fluxl a b= b:[ (replace a x) | x <- fluxl a b]