createControl('label','20.00','10.00','300.00','20.00','path number to be used to get to mine')
createControl('combo','20.00','30.00','100.00','20.00','cPathID',"0","path00","path01","path02","path03","path04","path05","path06","path07","path08","path09")
createControl('label','20.00','50.00','300.00','20.00','map name')
createControl('combo','20.00','70.00','100.00','20.00','cMapID',"1","Istaria","Spirit Isle")
createControl('label','20.00','90.00','300.00','20.00','path used to look for resources')
createControl('combo','20.00','110.00','100.00','20.00','cMinePathID',"3","path00","path01","path02","path03","path04","path05","path06","path07","path08","path09")
createControl('label','20.00','130.00','300.00','20.00','max distance to node')
createControl('edit','20.00','150.00','100.00','20.00','eMaxDistance','13')
createControl('label','20.00','170.00','300.00','20.00','gathering tool hotkey to be used')
createControl('combo','20.00','190.00','100.00','20.00','cToolID',"6","1","2","3","4","5","6","7","8","9")
createControl('label','20.00','210.00','300.00','20.00','search res. every n seconds ')
createControl('edit','20.00','230.00','140.00','20.00','eTimedStop','3')
createControl('checkbox','20.00','250.00','200.00','20.00','chTravel','-1','skip travel to mine')
createControl('checkbox','20.00','270.00','200.00','20.00','chGather','-1','skip gathering resources')
createControl('checkbox','20.00','290.00','200.00','20.00','chGetBack','-1','skip traveling back')
createControl('checkbox','20.00','310.00','200.00','20.00','chCraft','0','skip crafting items')
createControl('checkbox','20.00','330.00','200.00','20.00','chLoopFail','0','loop the sequence')
createControl('label','235.00','10.00','200.00','15.00','resources to gather')
createControl('memo','235.00','30.00','140.00','70.00','mResources','Mother lode\nRich Copper \nCopper and')
createControl('label','235.00','100.00','160.00','20.00','create item step 1 hotkey')
createControl('edit','235.00','120.00','140.00','20.00','eFormula1','5')
createControl('checkbox','235.00','140.00','200.00','20.00','chDeconstruct1','0','deconstruct item 1')
createControl('label','235.00','160.00','160.00','20.00','create item step 2 hotkey')
createControl('edit','235.00','180.00','140.00','20.00','eFormula2','8')
createControl('checkbox','235.00','200.00','200.00','20.00','chDeconstruct2','-1','deconstruct item 2')
createControl('label','235.00','220.00','160.00','20.00','path between machine 1 and 2')
createControl('combo','235.00','240.00','100.00','20.00','cPathItemID',"4","path00","path01","path02","path03","path04","path05","path06","path07","path08","path09")
createControl('label','235.00','265.00','160.00','20.00','resource to destoy when rooted')
createControl('edit','235.00','285.00','140.00','20.00','eBalast','Copper and Tin Ore')
