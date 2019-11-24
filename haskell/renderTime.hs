import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Calendar.WeekDate as WeekDate

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

renderFormat :: String -> Clock.UTCTime -> String
renderFormat format date = 
  let
    utcDay = Clock.utctDay date
    (yearNumber, numberOfWeek, numberOfDay) = WeekDate.toWeekDate utcDay
    (year, month, day) = Calendar.toGregorian $ utcDay
  in
    mconcat $ map (\s ->
      case s of
        -- TODO Implement leftpad
        'd' -> show $ day
        'D' -> case numberOfDay of
          1 -> "Mon"
          2 -> "Tue"
          3 -> "Wed"
          4 -> "Thu"
          5 -> "Fri"
          6 -> "Sat"
          7 -> "Sun"
          _ -> ""
        'j' -> show $ day
        'l' -> case numberOfDay of
          1 -> "Monday"
          2 -> "Tuesday"
          3 -> "Wednesday"
          4 -> "Thursday"
          5 -> "Friday"
          6 -> "Saturday"
          7 -> "Sunday"
          _ -> ""
        'N' -> show $ numberOfDay
        'S' -> ""
        'w' -> ""
        'z' -> ""
        'W' -> ""
        'F' -> ""
        'm' -> ""
        'M' -> ""
        'n' -> ""
        't' -> ""
        'L' -> ""
        'o' -> ""
        'y' -> drop 2 $ show $ year
        'Y' -> show $ year
        'a' -> ""
        'A' -> ""
        'B' -> ""
        'g' -> ""
        'G' -> ""
        'h' -> ""
        'H' -> ""
        'i' -> ""
        's' -> ""
        'u' -> ""
        'e' -> ""
        'I' -> ""
        'O' -> ""
        'P' -> ""
        'T' -> ""
        'Z' -> ""
        'c' -> ""
        'r' -> ""
        'U' -> ""
        _ -> [s]
      ) format

main = do
  time <- Clock.getCurrentTime
  print $ renderFormat "d D y Y" time