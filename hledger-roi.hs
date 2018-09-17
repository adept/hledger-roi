#!/usr/bin/env stack
-- stack --resolver lts-12.4 script
{-# LANGUAGE DeriveDataTypeable, ParallelListComp #-}
module Main ( main ) where

import Hledger

import Control.Exception ( bracket )
import Control.Monad
import System.Exit
import System.IO
import Data.Time.Calendar
import Text.Printf
import Data.List
import Data.Ord
import Statistics.Math.RootFinding
import Data.Decimal
import qualified Data.Text as T
import System.Console.CmdArgs
import Text.Tabular as Tbl
import qualified Text.Tabular.AsciiArt as Ascii

data Options = Options
  { cashFlow    :: Bool
  , debug       :: Bool
  , file        :: FilePath
  , investments :: String
  , pnl         :: String
  , begin       :: Maybe String
  , end         :: Maybe String
  , daily       :: Bool
  , weekly      :: Bool
  , monthly     :: Bool
  , yearly      :: Bool
  , period      :: Maybe String
  } deriving (Show, Data, Typeable)

queryFromOptions kind day accessor opts = 
  -- We ignore QueryOpts with fst as they are irrelevant for this program
  case parseQuery day (T.pack $ accessor opts) of
    (Any, _) -> error $ "you need to specify " ++ kind ++ " query"
    (query, []) -> query
    (_,     opts) -> error $ "this program cannot process query options " ++ show opts
    

intervalAndSpanFromOptions :: Day -> Options -> (Maybe Interval, DateSpan)
intervalAndSpanFromOptions date opts =
  let begin' = fmap parsedate $ begin opts
      end' = fmap parsedate $ end opts 
  in
   case (begin', end', daily opts, weekly opts, monthly opts, yearly opts, period opts) of
     (Nothing, Nothing, False, False, False, False, Nothing) -> (Nothing, nulldatespan)
     (b, e, True,  False, False, False, Nothing) -> (Just (Days 1),   DateSpan b e)
     (b, e, False, True,  False, False, Nothing) -> (Just (Weeks 1),  DateSpan b e)
     (b, e, False, False, True,  False, Nothing) -> (Just (Months 1), DateSpan b e)
     (b, e, False, False, False, True,  Nothing) -> (Just (Years 1),  DateSpan b e)
     (Nothing, Nothing, False, False, False, False, Just pexp) -> 
       let (i, s) = parsePeriodExpr' date (T.pack pexp) in (Just i, s)
     _ -> error "Cannot work with this combination of date/period flags"

options = 
  Options { 
     cashFlow = 
      False &= name "c"
      &= help "also show all revant transactions"
    , debug = 
      def 
      &= help "print debugging info"
    , file = 
      def &= name "f"
      &= typFile
      &= help "input ledger file (pass '-' for stdin)"
    , investments = 
      def &= name "i"
      &= typ "QUERY"
      &= help "investments query"
    , pnl = 
      def &= name "P"
      &= typ "QUERY"
      &= help "profit-and-loss query"
    , begin = 
      Nothing &= name "b"
      &= typ "DATE"
      &= help "calculate interest from this date"
    , end = 
      Nothing &= name "e"
      &= typ "DATE"
      &= help "calculate interest until this date"
    , daily =
      def &= name "D"
      &= help "calculate interest for each day"
    , weekly =
      def &= name "W"
      &= help "calculate interest for each week"
    , monthly = 
      def &= name "M"
      &= help "calculate interest for each month"
    , yearly = 
      def &= name "Y"
      &= help "calculate interest for each year"
    , period = 
      Nothing &= name "p"
      &= typ "PERIODEXP"
      &= help "set start date, end date, and/or report interval all at once (overrides the flags above)"
    } 
  &= program "hledger-roi"
  &= summary "compute return-on-investment for your portfolio using IRR and TWR"                             
  &= details [""]

main ::  IO ()
main = bracket (return ()) (\() -> hFlush stdout >> hFlush stderr) $ \() -> do
  opts <-cmdArgs options
  thisDay <- getCurrentDay
  
  let investmentsQuery = queryFromOptions "investments" thisDay investments opts
  let pnlQuery         = queryFromOptions "pnl" thisDay pnl opts

  jnl <- readJournalFile definputopts (file opts) >>= either fail return

  let trans = jtxns $ filterJournalTransactions investmentsQuery jnl
      
  when (null trans) $ do
    putStrLn "No relevant transactions found. Check your investments query"
    exitFailure
  
  when (debug opts) $ do
    putStrLn "DEBUG everything:"
    print $ trans
    putStrLn "DEBUG just non-PnL transactions:"
    print $ filter (matchesTransaction (And [investmentsQuery, Not pnlQuery])) trans
    putStrLn "DEBUG just PnL transactions:"
    print $ filter (matchesTransaction pnlQuery) trans
  
  let (requestedInterval, requestedSpan) = intervalAndSpanFromOptions thisDay opts
  let existingSpan = 
        let dates = map transactionDate2 trans in 
        DateSpan (Just $ minimum dates) (Just $ maximum dates)
  let wholeSpan = spanDefaultsFrom requestedSpan existingSpan 

  let spans = case requestedInterval of
        Nothing -> [wholeSpan]
        Just interval ->
            splitSpan interval $
            spanIntersect existingSpan wholeSpan

  tableBody <- (flip mapM) spans $ \(DateSpan (Just ibegin) (Just iend)) -> do
    -- Spans are [b,e)
    let valueBeforeThisPeriod =
          total trans (And [investmentsQuery, Date (openClosedSpan Nothing (Just ibegin))])
      
    let valueAtTheEndOfPeriod  = 
          total trans (And [investmentsQuery, Date (openClosedSpan Nothing (Just iend))])
        
    let cashFlowInThisPeriod = 
          calculateCashFlow trans (And [ Not (Or [investmentsQuery, pnlQuery]), 
                                         Date (openClosedSpan (Just ibegin) (Just iend)) ] )
    
    irr <- internalRateOfReturn opts ibegin iend valueBeforeThisPeriod valueAtTheEndOfPeriod cashFlowInThisPeriod
    twr <- timeWeightedReturn opts investmentsQuery trans ibegin iend valueBeforeThisPeriod valueAtTheEndOfPeriod cashFlowInThisPeriod
    let cashFlowAmt = negate $ sum $ map snd cashFlowInThisPeriod
    return [ showDate ibegin
           , showDate iend
           , show valueBeforeThisPeriod
           , show cashFlowAmt
           , show valueAtTheEndOfPeriod
           , show (valueAtTheEndOfPeriod - (valueBeforeThisPeriod + cashFlowAmt))
           , printf "%0.2f%%" irr
           , printf "%0.2f%%" twr ]

  let table = Tbl.Table 
              (Group NoLine (map (Header . show) (take (length tableBody) [1..]))) 
              (Group DoubleLine 
               [ Group SingleLine [Header "Begin", Header "End"]
               , Group SingleLine [Header "Value (begin)", Header "Cashflow", Header "Value (end)", Header "PnL"]
               , Group SingleLine [Header "IRR", Header "TWR"]])
              tableBody
  
  putStrLn $ Ascii.render id id id table

timeWeightedReturn opts investmentsQuery trans ibegin iend valueBefore valueAfter cashFlowInThisPeriod = do
  let initialUnitPrice = 100
  let initialUnits = valueBefore / initialUnitPrice
  let cashflow = map (\(d,a) -> (d, negate a)) $ filter ((/=0).snd) cashFlowInThisPeriod
    
  let units = 
        tail $
        (flip scanl) 
        (0,0,initialUnits)
        (\(_,_,unitBalance) (date, amt) -> 
          let valueOnDate = 
                total trans (And [investmentsQuery, Date (DateSpan Nothing (Just date))])
              unitPrice = if unitBalance == 0.0 then initialUnitPrice else valueOnDate / unitBalance
              unitsBoughtOrSold = amt / unitPrice
          in
           (unitsBoughtOrSold, unitPrice, unitBalance + unitsBoughtOrSold)
        )  
        cashflow
  
  let finalUnitBalance = if null units then initialUnits else let (_,_,u) = last units in u
      finalUnitPrice = valueAfter / finalUnitBalance
      totalTWR = roundTo 2 $ (finalUnitPrice - initialUnitPrice)
      years = (fromIntegral $ diffDays iend ibegin)/365 :: Double
      annualizedTWR = 100*((1+(realToFrac totalTWR/100))**(1/years)-1) :: Double
        
  let s d = show $ roundTo 2 d 
  when (cashFlow opts) $ do
    printf "\nTWR cash flow for %s - %s\n" (showDate ibegin) (showDate iend) 
    let (dates', amounts') = unzip cashflow
        (unitsBoughtOrSold', unitPrices', unitBalances') = unzip3 units
        dates = ibegin:dates'
        amounts = valueBefore:amounts'
        unitsBoughtOrSold = initialUnits:unitsBoughtOrSold'
        unitPrices = initialUnitPrice:unitPrices'
        unitBalances = initialUnits:unitBalances'
        
    putStr $ Ascii.render id id id 
      (Tbl.Table 
       (Group NoLine (map (Header . showDate) dates))
       (Group DoubleLine [ Group SingleLine [Header "Unit balance"] 
                         , Group SingleLine [Header "Cash", Header "Unit price", Header "Units"]
                         , Group SingleLine [Header "New Unit Balance"]])
       [ [oldBalance, amt, prc, udelta, balance] | oldBalance <- map s (0:unitBalances)
                                                 | balance <- map s unitBalances
                                                 | amt <- map s amounts
                                                 | prc <- map s unitPrices
                                                 | udelta <- map s unitsBoughtOrSold ])

    printf "Final unit price: %s/%s=%s U.\nTotal TWR: %s%%.\nPeriod: %.2f years.\nAnnualized TWR: %.2f%%\n\n" (s valueAfter) (s finalUnitBalance) (s finalUnitPrice) (s totalTWR) years annualizedTWR
  
  return annualizedTWR
  

internalRateOfReturn opts ibegin iend valueBefore valueAfter cashFlowInPeriod = do 
  let prefix = (ibegin, negate valueBefore)

      postfix = (iend, valueAfter)

      totalCF = sortBy (comparing fst) $ filter ((/=0) . snd) $ prefix : cashFlowInPeriod ++ [postfix]

  when (cashFlow opts) $ do
    printf "\nIRR cash flow for %s - %s\n" (showDate ibegin) (showDate iend) 
    let (dates, amounts) = unzip totalCF
    putStrLn $ Ascii.render id id id 
      (Tbl.Table 
       (Group NoLine (map (Header . showDate) dates))
       (Group SingleLine [Header "Amount"])
       (map ((:[]) . show) amounts))
                             
  -- 0% is always a solution, so require at least something here
  case ridders 0.00001 (0.000001,1000) (interestSum iend totalCF) of
    Root rate -> return ((rate-1)*100)
    _ -> error "Error: Failed to find solution."


openClosedSpan :: Maybe Day -> Maybe Day -> DateSpan
openClosedSpan md1 md2 = DateSpan (fmap (addDays 1) md1) (fmap (addDays 1) md2)


type CashFlow = [(Day, Quantity)]

interestSum :: Day -> CashFlow -> Double -> Double
interestSum referenceDay cf rate = sum $ map go cf
    where go (t,m) = (fromRational $ toRational m) * (rate ** (fromIntegral (referenceDay `diffDays` t) / 365))


calculateCashFlow :: [Transaction] -> Query -> CashFlow
calculateCashFlow trans query = map go trans
    where
    go t = (transactionDate2 t, total [t] query)

total :: [Transaction] -> Query -> Quantity
total trans query = unMix $ sumPostings $ filter (matchesPosting query) $ concatMap realPostings trans
    
unMix :: MixedAmount -> Quantity   
unMix a = 
  case (normaliseMixedAmount $ costOfMixedAmount a) of
    (Mixed [a]) -> aquantity a
    _ -> error "MixedAmount failed to normalize"

