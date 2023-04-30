# ReputationBlockchain

To Run code
You would need to have erlang installed on your machine.
- open terminal
- type erl
- c(globalblockchain).
- globalblockchain:start(NumOfNodes, Difficulty, ReputationThreshold)
- Ctrl + C to terminate

Replace NumOfNodes with number of desired nodes eg. 3
Replace Difficutly with desired difficulty eg. 4
Replace ReputationThreshold with a suitable value eg. 5

# Implementation
This is a hybrid reputation based blockchain solution.
A global blockchain and multiple regional blockchains are created which communicate with eachother.
Nodes can join either of the chains. The proposed solution can be read in more detail in the paper.

https://github.com/Ammar-Amjad/ReputationBlockchain/blob/main/Final_Report_Group17.pdf
