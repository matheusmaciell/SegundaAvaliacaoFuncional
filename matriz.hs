



--- precisa revisar esse $ transpose pq nao ta funcionando 
--matrixMult :: Num a => [[a]] -> [[a]] -> [[a]]
--matrixMult x y = matrixMultT x $ transpose y
 
matrixMultT :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultT [] _ = []
matrixMultT (a:as) b = calcRow a b : matrixMultT as b
 
calcRow :: Num a => [a] -> [[a]] -> [a]
calcRow _ [] = []
calcRow a (b:bs) = calcCell a b : calcRow a bs
 
calcCell :: Num a => [a] -> [a] -> a
calcCell col row = foldl1 (+) $ zipWith (*) col row


main:: IO()
main = do
	matriz1 <- getLine
	matriz2 <- getLine
	
	let resposta = matrixMultT (read matriz1) (read matriz2)
	print resposta
