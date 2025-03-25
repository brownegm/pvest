# slope is negative and the sma_model output is correct class

    Code
      print(testosm)
    Output
      Osmotic and pressure potential estimates: 
      -----------------------------------------------
      Osmotic potential at full turgor: -1.39 MPa 
      Pressure potential at full turgor: 1.39 MPa 
      Apoplastic fraction: 61.72 % 
      Number of hydration states used: 5 
      -----------------------------------------------

# Check that the errors work

    Code
      pvest::estOsmotic(data = test_df, wc.index = "rwd", wp.index = "psi", silent = T)
    Output
      Osmotic and pressure potential estimates: 
      -----------------------------------------------
      Osmotic potential at full turgor: -155.72 MPa 
      Pressure potential at full turgor: 155.72 MPa 
      Apoplastic fraction: 100 % 
      Number of hydration states used: 4 
      -----------------------------------------------

