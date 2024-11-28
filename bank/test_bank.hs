import Bank

-- Should output "-50"
test1 =
    let bankOp = do deposit 1000.0 
                    withdraw 500.0 
                    withdraw 550.0
                    b <- getBalance
                    return b in
    runBankOp bankOp

-- Should output "(200,-100)"
test2 :: (Float, Float)
test2 = let bankOp = do deposit 100.0
                        c <- withdraw 400.0
                        b <- getBalance
                        return (c, b) in
        runBankOp bankOp

main :: IO ()
main = do
    print test1
    print test2