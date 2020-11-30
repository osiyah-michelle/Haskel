# Haskel
programing Task 1
import Data.Char

--Michelle Osiyah
--maosiyah@syr.edu

--TASK1
--pt1

onLine :: (Float,Float) -> Float -> Float -> Float -> Bool
onLine (x1,y1) a b c = (a*x1) +(b*y1)+ c == 0
   

--determines whether the point (x1, y1) is on the line named by a · x + b · y + c = 0--
--tst1 onLine (2,7) 3 (-1) 1 evaluates to True
--tst2 onLine (2,16) 7 (-1) 2 evaluates to True
--tst3 onLine (1,10)  7 (-1) 2 evaluates to False
--tst4 onLine (6,4) 2 (-1) (-7) evaluates to False
--tst5 onLine (6,5) 2 (-1) (-7) evaluates to True

--pt2

degenerate :: Float -> Float -> Float -> Bool
degenerate a b c = a == 0 && b == 0
   

--determines whether the equation a · x + b · y + c = 0 is degenerate--
--tst1 degenerate 0 0 2 evaluates to True
--tst2 degenerate 0 1 2 evaluates to False
--tst3 degenerate 1 0 2 evaluates to False

--pt3

horizontal :: Float -> Float -> Float -> Bool
horizontal a b c = a == 0
  
--determines whether the equation a · x + b · y + c = 0 names a horizontal line. --
--tst1 horizontal 0 0 2 evaluates to True
--tst2  horizontal 3 0 2 evaluates to False
--tst3 horizontal 2 5 6 evaluates to False

--pt4

vertical   :: Float -> Float -> Float -> Bool
vertical a b c = b == 0 
   

--determines whether the equation a · x + b · y + c = 0 names a vertical line. --
--tst1 vertical  0 0 2 evaluates to True
--tst2  vertical  3 0 2 evaluates to True
--tst3 vertical  2 5 6 evaluates to False

--pt5
xIntercept :: Float -> Float -> Float -> Float
xIntercept a b c
    |not(a== 0) = (-c)/a
    |otherwise = a

--returns the x-coordinate of the x-intercept of the line named by a · x + b · y + c = 0.
--tst1 xIntercept  9 9 10  evaluates to (-1.1111112)
--tst2 xIntercept 3 0 2 evaluates to (-0.66666667)
--tst3 xIntercept  2 5 6 evaluates to (-3.0)

--pt6

yIntercept :: Float -> Float -> Float -> Float
yIntercept a b c
    |not(b== 0) =     (-c)/b 
    |otherwise = b

--returns the y-coordinate of the y-intercept of the line named by a · x + b · y + c = 0.
--tst1 yIntercept  3 4 5  evaluates to (-1.25)
--tst2 yIntercept  3 0 2 evaluates to (0.0)
--tst3 yIntercept  2 5 6 evaluates to (-1.2)

--pt7

parallel :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
parallel a1 b1 c1 a2 b2 c2 = a1*b2 == a2*b1


--tests whether the two lines named by the equations a1 · x + b1 · y + c1 = 0 and a2 · x + b2 · y + c2 = 0 are parallel.
--tst1 parallel  3 1 5 6 2 5  evaluates to True
--tst2 parallel  3 0 2 3 0 5  evaluates to True
--tst3 parallel  2 5 6 3 0 1 evaluates to False

--pt8
intersect :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
intersect  a1 b1 c1 a2 b2 c2 = not((a1 == 0)&& (b1 == 0) && (a2 ==0) && (b2==0)) && not((a1*b2) == (a2*b1))

{--
intersect :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
intersect  a1 b1 c1 a2 b2 c2 
    |(a1 == 0)&& (b1 == 0) && (a2 ==0) && (b2==0) = False
    |(a1*b2) == (a2*b1) = False
    |otherwise = True
--}
--tests whether the two lines named by the equations a1 · x + b1 · y + c1 = 0 and a2 · x + b2 · y + c2 = 0 intersect
--tst1 intersect  3 1 5 6 2 5  evaluates to False
--tst2 intersect  3 1 5 7 5 5  evaluates to True
--tst3 intersect  3 5 6 7 8 2  evaluates to True


--pt9
intersectPt ::  Float -> Float -> Float -> Float -> Float -> Float -> (Float,Float)
intersectPt a1 b1 c1 a2 b2 c2 
    |(a1*b2) == (a2*b1) = (0.0, 0.0)
    |otherwise =   (((((b1*c2)-(b2*c1))) / (((a1 * b2) - (a2*b1)))), ((((a2*c1) - (a1*c2)))/ (((a1*b2) - (a2*b1)))))

--returns the x-y-coordinates of the intersection point of the two lines named by the equations a1 · x + b1 · y + c1 = 0 and a2 · x + b2 · y + c2 = 0, when these lines do intersect
--tst1 intersectPt  3 1 5 6 2 5  evaluates to (0.0,0.0)
--tst2 intersectPt  3 1 5 7 5 5  evaluates to (-2.5, 2.5)
--tst3 intersectPt  3 5 6 7 8 2  evaluates to (3.4545455,-3.2727273)

{--
 x = (((b1*c2)-(b2*c1))) / (((a1 * b2) - (a2*b1)))
 y = (((a2*c1) - (a1*c2)))/ (((a1*b2) - (a2*b1)))
--}

--pt10
{--
lineEqual ::  Float -> Float -> Float -> Float -> Float -> Float -> Bool
lineEqual  a1 b1 c1 a2 b2 c2
    |(a1 == 0)&& (b1 == 0) || (a2 ==0) && (b2==0) = False
    |((a1 * b2)==(a2*b1))&& ((vertical a1 b1 c1) && (vertical a2 b2 c2)) && ((xIntercept a1 b1 c1)== (xIntercept a2 b2 c2))  = True
    |((a1 * b2)==(a2*b1)) && (not(vertical a1 b1 c1) && (vertical a2 b2 c2)) && ((yIntercept a1 b1 c1)== (yIntercept a2 b2 c2))  = True
    |otherwise = False
--}

lineEqual ::  Float -> Float -> Float -> Float -> Float -> Float -> Bool
lineEqual  a1 b1 c1 a2 b2 c2
    |(a1 == 0)&& (b1 == 0) || (a2 ==0) && (b2==0) = False
    |(a1 * b2)==(a2*b1) = helper1 a1 b1 c1 a2 b2 c2
    |otherwise = False

helper1 :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
helper1 a1 b1 c1 a2 b2 c2
    |(vertical a1 b1 c1) && (vertical a2 b2 c2) = helper2 a1 b1 c1 a2 b2 c2
    |otherwise = helper3 a1 b1 c1 a2 b2 c2

helper2 ::  Float -> Float -> Float -> Float -> Float -> Float -> Bool
helper2 a1 b1 c1 a2 b2 c2 = (xIntercept a1 b1 c1)== (xIntercept a2 b2 c2)
   

helper3 ::  Float -> Float -> Float -> Float -> Float -> Float -> Bool
helper3 a1 b1 c1 a2 b2 c2 = (yIntercept a1 b1 c1) == (yIntercept a2 b2 c2) 
   


--determines whether the lines named by the equations a1 · x + b1 · y + c1 = 0 and a2 · x + b2 · y + c2 = 0 are the same line. If either line is degenerate, then False is returned.

--tst1 lineEqual  3 1 5 6 2 10  evaluates to True
--tst2 lineEqual  3 1 5 7 5 5  evaluates to False
--tst3 lineEqual  3 5 6 6 10 12  evaluates to True


