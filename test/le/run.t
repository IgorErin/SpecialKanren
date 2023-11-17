  $ ./run.exe
  le false, 10 answers {
  q=_.10 [=/= O]; r=O;
  q=S (_.12 [=/= O]); r=S (O);
  q=S (S (_.14 [=/= O])); r=S (S (O));
  q=S (S (S (_.16 [=/= O]))); r=S (S (S (O)));
  q=S (S (S (S (_.18 [=/= O])))); r=S (S (S (S (O))));
  q=S (S (S (S (S (_.20 [=/= O]))))); r=S (S (S (S (S (O)))));
  q=S (S (S (S (S (S (_.22 [=/= O])))))); r=S (S (S (S (S (S (O))))));
  q=S (S (S (S (S (S (S (_.24 [=/= O]))))))); r=S (S (S (S (S (S (S (O)))))));
  q=S (S (S (S (S (S (S (S (_.26 [=/= O])))))))); r=S (S (S (S (S (S (S (S (O))))))));
  q=S (S (S (S (S (S (S (S (S (_.28 [=/= O]))))))))); r=S (S (S (S (S (S (S (S (S (O)))))))));
  }
  le true, 10 answers {
  q=O; r=_.11;
  q=S (O); r=S (_.13);
  q=S (S (O)); r=S (S (_.15));
  q=S (S (S (O))); r=S (S (S (_.17)));
  q=S (S (S (S (O)))); r=S (S (S (S (_.19))));
  q=S (S (S (S (S (O))))); r=S (S (S (S (S (_.21)))));
  q=S (S (S (S (S (S (O)))))); r=S (S (S (S (S (S (_.23))))));
  q=S (S (S (S (S (S (S (O))))))); r=S (S (S (S (S (S (S (_.25)))))));
  q=S (S (S (S (S (S (S (S (O)))))))); r=S (S (S (S (S (S (S (S (_.27))))))));
  q=S (S (S (S (S (S (S (S (S (O))))))))); r=S (S (S (S (S (S (S (S (S (_.29)))))))));
  }

