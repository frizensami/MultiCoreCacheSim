stack build &&
echo -e "\nBuild Complete\n-------------\nStarting cs4223-as2-exe..\n" &&
stack exec cs4223-as2-exe Illinois data/test_loadone_four/test 1024 1 16
