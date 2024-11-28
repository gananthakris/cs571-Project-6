module Bank where

newtype BankOp a = BankOp { runBankOp' :: Float -> (a, Float) }

-- Make BankOp a monad to work with do notation in the tests
instance Functor BankOp where
    fmap f (BankOp g) = BankOp (\balance -> let (x, newBalance) = g balance in (f x, newBalance))

instance Applicative BankOp where
    pure x = BankOp (\balance -> (x, balance))
    BankOp f <*> BankOp g = BankOp (\balance ->
        let (func, balance1) = f balance
            (x, balance2) = g balance1
        in (func x, balance2))

instance Monad BankOp where
    return = pure
    BankOp m >>= f = BankOp (\balance ->
        let (x, newBalance) = m balance
            BankOp m' = f x
        in m' newBalance)

-- Deposit money into account
deposit :: Float -> BankOp ()
deposit amount = BankOp (\balance -> ((), balance + amount))

-- Withdraw with $100 overdraft limit
withdraw :: Float -> BankOp Float
withdraw amount = BankOp (\balance ->
    let newBalance = balance - amount
        actualWithdrawal = if newBalance < -100
                          then balance + 100  -- Only withdraw up to overdraft limit
                          else amount
    in (actualWithdrawal, balance - actualWithdrawal))

-- Get current balance
getBalance :: BankOp Float
getBalance = BankOp (\balance -> (balance, balance))

-- Run a bank operation starting with 0 balance
runBankOp :: BankOp a -> a
runBankOp (BankOp f) = fst $ f 0