program test

      print *, epsilon(1.0_8)
      print *, epsilon(1000.0_8)
      print *, 2.0_8**(-52);
      print *, tiny(1.0_8)
      print *, tiny(1000.0_8)
      print *, 2.0_8**(-1022)
end program test
