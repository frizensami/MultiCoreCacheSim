stack build  &&
echo -e "\nBuild Complete\n-------------\nStarting cs4223-as2-exe..\n" &&
stack exec cs4223-as2-exe Illinois data/bodytrack_four/bt 1024 1 32
