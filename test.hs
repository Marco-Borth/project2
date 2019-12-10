import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.OrdinalDate

(year, month, day) = (1981, 6, 16)

t = do printf "%d/%d/%d was a %s\n" year month day week_day_name
       printf "%d/%d/%d was day %d of the week %d\n" year month day week_day week
       printf "%d/%d/%d was day %d of month %d\n" year month day month_day month_
       printf "%d/%d/%d was day %d of year %d\n" year month day year_day year_
    where date = (fromGregorian year month day)
          (week, week_day) = sundayStartWeek date
          (_, month_, month_day) = toGregorian date
          (year_, year_day) = toOrdinalDate date
          (week_day_name, _) = wDays defaultTimeLocale !! week_day
-- 1981/6/16 was a Tuesday
-- 1981/6/16 was day 2 of the week 24
-- 1981/6/16 was day 16 of month 6
-- 1981/6/16 was day 167 of year 1981
