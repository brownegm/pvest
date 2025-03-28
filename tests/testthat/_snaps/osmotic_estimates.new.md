# slope is negative and the sma_model output is correct class

    Code
      print(testosm)
    Output
      Osmotic and pressure potential estimates: 
      -----------------------------------------------
      Osmotic potential at full turgor: -0.03 MPa 
      Pressure potential at full turgor: 0.03 MPa 
      Apoplastic fraction: 99.28 % 
      Number of hydration states used: 5 
      -----------------------------------------------

# Check that the errors work

    Code
      pvest::estOsmotic(data = test_df, wc.index = "rwd", wp.index = "psi", silent = T)
    Output
      Osmotic and pressure potential estimates: 
      -----------------------------------------------
      Osmotic potential at full turgor: -0.76 MPa 
      Pressure potential at full turgor: 0.76 MPa 
      Apoplastic fraction: 103.06 % 
      Number of hydration states used: 4 
      -----------------------------------------------

