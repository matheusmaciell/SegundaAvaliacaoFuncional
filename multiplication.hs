import Data.List
import Control.Parallel
import Control.Exception
import Data.Time

main:: IO()
main = do
 putStrLn "Escolha uma das opções a seguir:"
 putStrLn "1: Multiplicacao sequencial"
 putStrLn "2: Multiplicacao com paralelismo"
 putStrLn "3: Comparacao do sequencial com o paralelismo"


 op <- getLine
 if(op == "1") then do
 putStrLn "Digite a primeira matriz no seguindo formato: [[1,1],[1,1]] para uma matriz 2x2"
 a <- getLine
 putStrLn "Digite a segunda no mesmo formato."
 b <- getLine
 let r = multiplyMatrix  (Matrix (read a)) (Matrix (read b))
 print r
 else if(op == "2") then do
 putStrLn "Digite a primeira matriz no seguindo formato: [[1,1],[1,1]] para uma matriz 2x2"
 a <- getLine
 putStrLn "Digite a segunda no mesmo formato."
 b <- getLine
 let r = mult (read a) (transpose (read b))
 print r
 else do
 putStrLn "Digite a primeira matriz no seguindo formato: [[1,1],[1,1]] para uma matriz 2x2"
 a <- getLine
 putStrLn "Digite a segunda no mesmo formato."
 b <- getLine
 startSeq <- getCurrentTime
 evaluate (sum [1 .. 1000000])
 let v = multiplyMatrix (Matrix (read a)) (Matrix (read b))
 endSeq <- getCurrentTime
 startPar <- getCurrentTime
 evaluate (sum [1 .. 1000000])
 let r = mult (read a) (read b)
 endPar <- getCurrentTime
 putStr "Tempo execução paralelo: "
 print (diffUTCTime endPar startPar)
 putStr "Tempo execução sequencial: "
 print(diffUTCTime endSeq startSeq)
 putStrLn "End"

data Matrix a = Matrix [[a]] | Nil
              deriving (Eq,Read)

instance (Num a)=>Num (Matrix a) where
 (*) _ Nil = Nil
 (*) Nil _ = Nil
 (*) (Matrix x) (Matrix y) = multiplyMatrix (Matrix x) (Matrix y)
 (+) _ Nil = Nil
 (+) Nil _ = Nil
 (-) _ Nil = Nil
 (-)	Nil _ = Nil
instance (Show a) => Show (Matrix a) where
  show (Matrix a) = printString (Matrix a)
  show (Nil) = "Nil"

verificaMultiplicacao:: (Matrix a) -> (Matrix a) -> Bool
verificaMultiplicacao a b
    | (snd(getDimension a)) == (fst( getDimension b)) = True
    |otherwise = False

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

repeatMatrix :: Int -> Int -> a ->  (Matrix a)
repeatMatrix row col value | row > 0 && col > 0 = Matrix (replicate row (replicate col value))
               | otherwise = Nil

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
fromList :: Int -> Int -> [a] -> Matrix a
fromList _ _ [] = Nil
fromList row col (x:xs) | length(x:xs) >= row*col = generateMatrix row col (\(i,j)-> (x:xs)!!((i-1)*col+(j-1)))
            | otherwise = Nil

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

parHelp :: ( Num a ) => [ a ] -> [ a ] -> a
parHelp [] [] = 0
parHelp ( x : xs ) ( y : ys ) = ret where
        ret = par a ( pseq a ( a + parHelp xs ys ) ) where
            a = x * y
-- a funcao "par" incia a computacao em paralelo
-- ja a pseq 
--como vantagens, o programador nao necessita ter conhecimento de como ocorre a avaliacao preguiçosa nem conhecer o funcionamento do coletor do lixo para gerar paralelismo
helpMult :: ( Num a ) => [ a ] -> [ [ a ] ] -> [ a ]
helpMult _ [] = []
helpMult x ( y : ys ) = ret where
     ret =  par a ( pseq a  ( a : helpMult x ys ) ) where
       a = parHelp x y
mult :: ( Num a ) => [ [ a ] ] -> [ [ a ] ] -> [ [ a ] ]
mult [] _ = []
mult ( x : xs ) ys = ret where
     ret = par a ( pseq a  ( a : mult xs ys ) ) where
        a = helpMult x ys
