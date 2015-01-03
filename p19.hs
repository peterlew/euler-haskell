
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
daysOfWk = ["Mon", "Tues", "Weds", "Thurs", "Fri", "Sat", "Sun"]
numDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
numDaysLeap = [31, 29] ++ drop 2 numDays

calendar :: Int -> [(String, Int, Int)]
calendar yr = let dys = if mod yr 4 == 0 && (mod yr 100 > 0 || mod yr 400 == 0) then numDaysLeap
                        else numDays
              in foldl (++) [] $ zipWith (\m n -> map (\l -> (m, l, yr)) [1..n]) months dys

calendarSince yr | yr > 2000 = []
calendarSince yr = calendar yr ++ calendarSince (yr + 1)

centuryDays = zipWith (\(m, l, yr) day -> (day, m, l, yr)) (calendarSince 1900) (repeat' daysOfWk)
 where repeat' l = l ++ repeat' l

p19 = length $ filter (\(day, m, l, yr) -> yr > 1900 && l == 1 && day == "Sun") centuryDays