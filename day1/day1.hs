 data Option a = Some a | None
 
 mishow (Some a) = show a
 mishow None = "none"
 
 ctoi :: String -> Option Integer
 ctoi ('0':_) = Some 0
 ctoi ('1':_) = Some 1
 ctoi ('2':_) = Some 2
 ctoi ('3':_) = Some 3
 ctoi ('4':_) = Some 4
 ctoi ('5':_) = Some 5
 ctoi ('6':_) = Some 6
 ctoi ('7':_) = Some 7
 ctoi ('8':_) = Some 8
 ctoi ('9':_) = Some 9
 ctoi _ = None
 
 mistoi :: String -> Option Integer
 mistoi ('o':'n':'e':_) = Some 1
 mistoi ('t':'w':'o':_) = Some 2
 mistoi ('t':'h':'r':'e':'e':_) = Some 3
 mistoi ('f':'o':'u':'r':_) = Some 4
 mistoi ('f':'i':'v':'e':_) = Some 5
 mistoi ('s':'i':'x':_) = Some 6
 mistoi ('s':'e':'v':'e':'n':_) = Some 7
 mistoi ('e':'i':'g':'h':'t':_) = Some 8
 mistoi ('n':'i':'n':'e':_) = Some 9
 mistoi inp = ctoi inp
 
 gfirst :: (String -> Option Integer) -> Integer -> String -> Integer
 gfirst _ d [] = d
 gfirst f d inp = case f inp of
                     Some p -> p
                     None -> gfirst f d (tail inp)
 
 glast :: (String -> Option Integer) -> Integer -> String -> Integer
 glast _ d [] = d
 glast f d inp = case f inp of
                     Some p -> glast f p (tail inp)
                     None -> glast f d (tail inp)
 
 doline :: (String -> Option Integer) -> String -> Integer
 doline f line = (gfirst f 0 line) * 10 + (glast f 0 line)
 
 doall :: (String -> Option Integer) -> [String] -> Integer
 doall f inpLines = sum (map (doline f) inpLines)
 
 main :: IO ()
 main = do
     content <- readFile "input.txt"
     print (doall ctoi (lines content))
     print (doall mistoi (lines content))
