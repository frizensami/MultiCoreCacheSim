stack build && #--executable-profiling --library-profiling &&
echo -e "\nBuild Complete\n-------------\nStarting cs4223-as2-exe..\n" &&
stack exec cs4223-as2-exe MESI data/blackscholes_four/blackscholes 1024 1 16 # -- +RTS -p -RTS
