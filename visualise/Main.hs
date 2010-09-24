-- | Generate some visualisations of map state

module Main where

import qualified Graphics.Rendering.Diagrams as D
import PlanetWars
import StringLike

do_main diag = D.renderAs D.PNG "vis.png" (D.Width 400) diag


vis = D.vcatA D.hcenter $ zipWith D.text sizes letters

-- --------------------------------------------------------

snellen = D.vcatA D.hcenter $ zipWith D.text sizes letters

sizes = [36,24,18,12,9,6,5,4,3]
letters = ["A","D F","H Z P","T X U D","Z A D N H","P N T U H X"
          ,"U A Z N F D T","N P H T A F X U","X D F H P T Z A N"
          ,"F A X T D N H U P Z"]

-- ---------------------------------------------------------------------

--fVisBot :: BotFunction  -- [Planet] -> [Fleet] -> [Fleet]
--playAs' :: (Bot a, S.StringLike string) => a -> string -> [String]

fVisBot =
  let 
    (planets,fleets) = head $ parseGameState $ concat mapStr                      
    --diag = vis
    diag = D.position $ map (\p -> (posToPoint (position p),render_planet p)) planets
  in 
    --show (planets,fleets)
    do_main diag

posToPoint (Position x y) = (x,-y)

pidToNum (PlanetID i) = i

render_planet planet =
  D.union [
    D.lc (planetColour planet) $ D.circle (0.5 + 0.3*(fromIntegral (production planet)))
    , D.vcat [D.text 0.6 (show $ fromIntegral (ships planet))
             , D.hcat [D.text 0.6 (show $ pidToNum (planetID planet))
                      , D.text 0.6 (show $ fromIntegral (production planet))
                      ]
             ]
    ]  
  where
    planetColour p
      | isNeutral p = D.grey
      | isMine    p = D.blue
      | isEnemy   p = D.red
  
-- ---------------------------------------------------------------------

--main = fVisBot

-- ---------------------------------------------------------------------

mapStr = [
   "P 10.840348964 11.5332290838 0 114 3\n",
   "P 0.576108731347 22.7745040746 1 100 5\n",
   "P 21.1045891966 0.291954092956 2 100 5\n",
   "P 18.0562350009 1.72809803672 0 3 5\n",
   "P 3.6244629271 21.3383601308 0 3 5\n",
   "P 11.3799713178 20.847530038 0 8 3\n",
   "P 10.3007266102 2.21892812953 0 8 3\n",
   "P 16.0512166362 19.0871026134 0 16 1\n",
   "P 5.62948129175 3.97935555418 0 16 1\n",
   "P 11.1591909212 16.8074649675 0 72 3\n",
   "P 10.5215070068 6.25899320004 0 72 3\n",
   "P 3.95581121455 0.0 0 74 5\n",
   "P 17.7248867134 23.0664581676 0 74 5\n",
   "P 6.84895300719 5.1007588417 0 56 3\n",
   "P 14.8317449208 17.9656993259 0 56 3\n",
   "P 0.0 5.86108744956 0 85 2\n",
   "P 21.680697928 17.205370718 0 85 2\n",
   "P 19.9963643377 2.67917262788 0 24 1\n",
   "P 1.68433359029 20.3872855397 0 24 1\n",
   "P 3.33979657574 18.9410220747 0 88 3\n",
   "P 18.3409013522 4.12543609287 0 88 3\n",
   "P 6.77101934829 13.1633290558 0 89 2\n",
   "P 14.9096785797 9.90312911175 0 89 2\n",
   "go\n"
   ]

-- EOF
