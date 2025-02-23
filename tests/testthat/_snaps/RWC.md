# Check that the errors work

    Code
      estRWC(data = data, fw.index = 1, wp.index = 2, dm.index = 3, n_row = 4,
        silent = F)
    Output
      
      Estimating RWC and RWD...
      
               fw         wp  dm
      1 0.4500000 -0.5000000 0.5
      2 0.4222222 -0.7222222 0.5
      3 0.3944444 -0.9444444 0.5
      4 0.3666667 -1.1666667 0.5
      5 0.3388889 -1.3888889 0.5
      6 0.3111111 -1.6111111 0.5
      Using the following columns for the estimation:
      {Fresh mass}: fw
      {Water potential}: wp
      
      $swm
      [1] 0.5125
      
      $swc
      [1] 1.025
      
      $rwc
       [1] 87.80488 82.38482 76.96477 71.54472 66.12466 60.70461 55.28455 49.86450
       [9] 44.44444 39.02439
      
      $rwd
       [1] 12.19512 17.61518 23.03523 28.45528 33.87534 39.29539 44.71545 50.13550
       [9] 55.55556 60.97561
      
      attr(,"units")
      [1] "g"   "g/g" "%"   "%"  
      attr(,"flag")
      [1] "There are 0 potential plateau points."
      attr(,"class")
      [1] "rwcEst"

