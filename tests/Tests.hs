module Main (main) where

import System.Locale (defaultTimeLocale)
import System.Time
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testCase "T5430" t5430
  , testCase "time002" time002
  , testCase "time003" time003
  , testCase "time004" time004
  , testCase "issue18" issue18
  ]

t5430 :: Assertion
t5430 = do
  let clockTime = TOD 32400 0 -- 00:00:00 on 1 Jan 1970
  calTime <- toCalendarTime clockTime
  -- We check for 001 or 365 (timezone locale will determine which one)
  -- and output 001 for testing output consistently.
  let j = formatCalendarTime defaultTimeLocale "%j" calTime
  assertBool "Either 001 or 365" $
    j `elem` ["001", "365"]

time002 :: Assertion
time002 = do
  t <- getClockTime
  let clock = show t
  c <- toCalendarTime t
  let cal = calendarTimeToString c
  let t2 = toClockTime c
      clock2 = show t2
  assertEqual "calendarTimeToString . toCalendarTime = id" clock cal
  assertEqual "show . toClockTime . toCalendarTime = id" clock clock2

time003 :: Assertion
time003 = do
  time <- getClockTime
  assertBool "does show look plausible?" $
    plausible (show time)

  let (CalendarTime year month mday hour minute sec _psec
                    wday _yday timezone _gmtoff _isdst) = toUTCTime time
      time2 = wdays !! fromEnum wday ++
        (' ' : months !! fromEnum month) ++
        (' ' : shows2 mday (' ' : shows2 hour (':' : shows2 minute (':' : shows2 sec
        (' ' : timezone ++ ' ' : shows year "\n")))))
  assertBool "does time2 look plausible?" $
    plausible time2

  where
    wdays = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    shows2 x = showString (pad2 x)
    pad2 x = case show x of
               c@[_] -> '0' : c
               cs -> cs

    plausible str = filter (== ':') str == "::"

time004 :: Assertion
time004 = do
  time <- getClockTime
  let (CalendarTime year month mday hour minute sec psec
                    wday yday timezone gmtoff isdst) = toUTCTime time
      time' = toClockTime (CalendarTime (year - 1) month mday hour minute sec psec
                           wday yday timezone gmtoff isdst)
  assertEqual "should have same length"
    (length (show time)) (length (show time'))

issue18 :: Assertion
issue18 = do
  let c = TOD ((19078*24+2)*3600+1357) 0
  assertEqual "noTimeDiff should not make a diff"
    c (addToClockTime noTimeDiff c)
