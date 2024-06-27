# JASA_Sims
All the tables in the main paper (Tables 1, 2, and 4), and Table 3 in the supplementary article can be reproduced.
We first describe the folders and files.

 
We now describe D4RL folder, it is used to reproduce Tables 1 and 2 in the main paper. D4RL has four environments: hopper, halfcheetah, maze and walker, each environment has a folder therein. There are 9 methods in the table: P1 is our proposed model-free method; DM1 and DM2 are direct methods; IS1, IS2, and IS3 are importance sampling methods; DR1, DR2, and DR3, are doubly reinforcement learning methods. Each method has its own file, for example, P1 and DM1 are implemented by P1.R and DM1.R files, respectively. All other files and folders are auxiliary folders and files. To reproduce each method in the table, one only need to run the files with their corresponding names. We note that P2 and MB are not included in the D4RL folder.

The ``sensitivity'' folder is used to reproduce the sensitivity analysis of Table 4 in Section 6. There are 11 methods in the table: In addition to the 9 methods mentioned above, P2 and MB are included. Similarly, to reproduce each method in the table, one only need to run the files with their corresponding names.  

Mixture folder. Mixture folder is used to reproduce Table 3 in the supplementary article.  

We next illustrate how to reproduce these tables. 

Tables 1, and 2 in the main paper (D4RL): for example, to obtain the result of P1 for Maze-2D in Table 1, simply open P1.R file in the D4RL/Maze folder, then run the code. Similarly, to obtain the results of other methods, please open the corresponding R files and run the code therein.


Table 4 (sensitivity analysis) in the main: for example, to obtain the result of P1 in the table. Simply open P1.R file in the Sensitivity folder, then run the code. Similarly, to obtain the results of other methods, please open the corresponding R files and run the code therein.
    

Mixture folder for Table 3 in the supplementary article can be reproduced analogously, we omit the details to save space.

Finally, we recommend downloading the JASA\_Sims folder to the desktop, as in all the provided codes, we load or source the R files using the following path: ``~/Desktop/JASA\_Sims/''.
