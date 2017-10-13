stack build &&
echo -e "\nBuild Complete\n-------------\nStarting cs4223-as2-exe..\n" &&
stack exec cs4223-as2-exe MESI data/bodytrack_four/bodytrack 1024 1 16
