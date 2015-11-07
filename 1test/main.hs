import Sol1

putStrShow = putStr.show

main = do
	let (ch1, ch2) = resNumbers
	putStrShow $ ch1
	putStr "x"
	putStrShow $ ch2
	putStr "="
	putStrShow $ ch1*ch2
	putStrLn ""