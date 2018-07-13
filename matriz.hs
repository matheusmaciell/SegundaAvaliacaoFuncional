import Data.List
import System.Directory

data Matrix a = Matrix [[a]] | Nil
              deriving (Eq)


instance (Show a) => Show (Matrix a) where
	show (Matrix a) = printString (Matrix a)
	show (Nil) = "Nil"

instance (Num a)=>Num (Matrix a) where
	(*) _ Nil = Nil
	(*) Nil _ = Nil
	(*) (Matrix x) (Matrix y) = multiplyMatrix (Matrix x) (Matrix y)
	(+) (Matrix x) (Matrix y) = addMatrix (Matrix x) (Matrix y)
	(+) _ Nil = Nil
	(+) Nil _ = Nil
	(-) (Matrix x) (Matrix y) = subMatrix (Matrix x) (Matrix y)
	(-) _ Nil = Nil
	(-)	Nil _ = Nil

main:: IO()
main = do 


	let a = Matrix [[1,2],[3,2]]
	let b = Matrix [[1,2],[1,1]]
	
	let confirmacao = verificaMultiplicacao a b
	
	if(confirmacao) then do 
		printMatrix (multiplyMatrix a b)
	else do
		putStrLn "Formato invalido"
	
	
verificaMultiplicacao:: (Matrix a) -> (Matrix a) -> Bool
verificaMultiplicacao a b
		| (snd(getDimension a)) == (fst( getDimension b)) = True
		|otherwise = False

removeMatrix (Matrix a) = a

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

repeatMatrix :: Int -> Int -> a ->  (Matrix a)
repeatMatrix row col value | row > 0 && col > 0 = Matrix (replicate row (replicate col value))
						   | otherwise = Nil


printMatrix :: Show a => Matrix a -> IO()
printMatrix Nil = putStr ("~Undefined Matrix")
printMatrix (Matrix matrix) = putStr(printString (Matrix matrix) )

printString :: Show a => Matrix a -> String
printString Nil = ""
printString (Matrix matrix) = ("\n->("++show(length(matrix))++","++show(length(matrix!!0))++") matrix\n"++ printMatrixHelper (Matrix matrix) 0)

printMatrixHelper :: Show a => Matrix a -> Int -> String
printMatrixHelper (Matrix matrix) row | row < length(matrix) = show (matrix!!row) ++ "\n" ++ printMatrixHelper (Matrix matrix) (row+1)
					   				  | otherwise = "\n"

buildRow :: (Int,Int) -> Int -> Int -> ((Int,Int) -> a) -> [a]
buildRow (row,col) travR travC func | travC < col = [func (travR+1,travC+1)] ++ buildRow (row,col) travR (travC+1) func
								 	| travR+1 < row = buildRow (row,col) (travR+1) 0 func
								 	| otherwise = []


getDimension :: Matrix a -> (Int,Int)
getDimension Nil = (0,0)
getDimension (Matrix matrix) = (length(matrix),length(matrix!!0))

generateMatrix :: Int -> Int -> ((Int,Int) -> a) -> Matrix a
generateMatrix row col genFunc =  Matrix (splitEvery col (buildRow (row,col) 0 0 genFunc))


--identityMatrix :: Int -> Matrix Int
identityMatrix dim | dim > 0 = generateMatrix dim dim (\(i,j) -> fromIntegral (fromEnum (i == j)))
				   | otherwise = Nil


scalarMult :: (Num a) => a -> Matrix a -> Matrix a
scalarMult _ Nil = Nil
scalarMult scalar (Matrix matrix) = generateMatrix (length matrix) (length (matrix!!0)) (\(i,j)-> ( (matrix!!(i-1))!!(j-1) ) * scalar)


diagonalList :: Num a => Int -> [a] -> Matrix a 
diagonalList _ [] = Nil
diagonalList n ls | n <= len = generateMatrix n n (\(i,j)-> (fromIntegral (fromEnum (i==j))*(ls!!(i-1))))
				  | n > len = diagonalList n (ls ++ (replicate diff zero))
				  where
				  	len = length ls
				  	diff = n - len
				  	zero = (ls!!0) - (ls!!0)


printMatrixList :: Show a =>  [[[a]]] -> IO()
printMatrixList [] = putStrLn "~Empty List"
printMatrixList (x:xs) = putStrLn ( (intercalate " " (map (\x -> printString (Matrix x)) (x:xs))) ++ show(length(x:xs))++" permutations")


permMatrix :: Int -> IO()
permMatrix 0 = putStrLn "~Matrix of dimension 0 DNE"
permMatrix dim = printMatrixList (permutations (removeMatrix (identityMatrix dim)) )

fromList :: Int -> Int -> [a] -> Matrix a
fromList _ _ [] = Nil
fromList row col (x:xs) | length(x:xs) >= row*col = generateMatrix row col (\(i,j)-> (x:xs)!!((i-1)*col+(j-1)))
						| otherwise = Nil

indexMatrix :: (Num a) => Int -> Int -> Matrix a -> a
indexMatrix row col (Matrix matrix) | row > 0 && row <= length(matrix) && col > 0 && col <= length (matrix!!0) = (matrix!!(row-1))!!(col-1)
					       			| otherwise = zero
					       			where 
					       				zero = ((matrix!!0)!!0)-((matrix!!0)!!0)

getRow :: Int -> Matrix a -> [a]
getRow _ Nil = []
getRow row (Matrix matrix) | row < length(matrix) = matrix!!row
				  		   | otherwise = []
getCol :: Int -> Matrix a -> [a]
getCol _ Nil = []
getCol col (Matrix matrix) | col < length(matrix!!0) = getColHelper matrix 0 col
				  		   | otherwise = []

getColHelper :: [[a]] -> Int -> Int -> [a]
getColHelper matrix row col | row < length(matrix) = [(matrix!!row)!!col] ++ getColHelper matrix (row+1) col
							| otherwise = []

multiplyMatrix :: (Num a)=> Matrix a -> Matrix a -> Matrix a
multiplyMatrix Nil _ = Nil
multiplyMatrix _ Nil = Nil
multiplyMatrix (Matrix m1) (Matrix m2) | c1 == r2 = fromList r1 c2 (multiplyMatrixHelper 0 0 m1 m2)
					 				   | otherwise = Nil
					where
						(r1,c1) = getDimension (Matrix m1)
						(r2,c2) = getDimension (Matrix m2)

multiplyMatrixHelper :: (Num a) => Int -> Int -> [[a]]->[[a]] -> [a]
multiplyMatrixHelper row col m1 m2 | col < length(m2!!0) = [sum(zipWith (*) m1Row m2Col)] ++ multiplyMatrixHelper row (col+1) m1 m2
								   | row < length(m1) = multiplyMatrixHelper (row+1) 0 m1 m2
								   | otherwise = []
										where
											m1Row = getRow row (Matrix m1)
											m2Col = getCol col (Matrix m2)

addMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
addMatrix Nil _ = Nil
addMatrix _ Nil = Nil
addMatrix (Matrix m1) (Matrix m2) | r1 == r2 && c1 == c2 = Matrix (addMatrixHelper m1 m2 0)
								  | otherwise = Nil
									where
										(r1,c1) = getDimension (Matrix m1)
										(r2,c2) = getDimension (Matrix m2)

addMatrixHelper :: (Num a) => [[a]] -> [[a]] -> Int -> [[a]]
addMatrixHelper m1 m2 row | row < length(m1) = [zipWith (+) m1Row m2Row] ++ addMatrixHelper m1 m2 (row+1)
						  | otherwise = []
						  where 
						  	m1Row = getRow row (Matrix m1)
						  	m2Row = getRow row (Matrix m2)

subMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
subMatrix Nil _ = Nil
subMatrix _ Nil = Nil
subMatrix (Matrix m1) (Matrix m2) = addMatrix (Matrix m1) (scalarMult (-1) (Matrix m2))


getSublist :: [a] -> Int -> [a]
getSublist [] _ = []
getSublist (x:xs) skip | skip == 0 = (x:xs)
					   | otherwise = getSublist xs (skip-1)






	
	
	
	

