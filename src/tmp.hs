

rnd::Double ->Double
rnd x = (fromIntegral (floor (x*1000.0)))/1000.0

main = putStr $ show $ rnd 0.3456