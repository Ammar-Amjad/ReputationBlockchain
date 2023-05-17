# Reputation Blockchain

To Run code
You would need to have erlang installed on your machine.
- open terminal
- type erl
- c(globalblockchain).
- globalblockchain:start(NumOfNodes, Difficulty, ReputationThreshold).
- Ctrl + C to terminate

Replace NumOfNodes with number of desired nodes eg. 3
Replace Difficutly with desired difficulty eg. 4
Replace ReputationThreshold with a suitable value eg. 5

Please see SampleExection.png for a sample on how to run the code.

https://github.com/Ammar-Amjad/ReputationBlockchain/blob/main/SampleExecution.png

# Abstract
This paper investigates the blockchain trilemma,
a challenge in achieving simultaneous security, scalability, and
decentralization in blockchain technology. We explore various
second-layer solutions, such as sharding, side chains, rollups, and
plasma, while discussing their limitations. Our proposed hybrid
solution combines reputation-based consensus mechanisms and
geo-based layer 2 solutions to address the trilemma effectively.
The two-layer blockchain system includes a global blockchain
and multiple regional sidechains, which enhance scalability,
security, and localization while preserving decentralization and
transparency. Reputation points incentivize miners to contribute
to regional chains, and an optimization algorithm summarizes
transactions for increased efficiency. Our implementation, written
in Erlang, demonstrates significant improvements in transaction
throughput and reduced block time compared to traditional
blockchains. In conclusion, this hybrid approach provides a
promising solution to the blockchain trilemma, paving the way
for future advancements in the field.

# Implementation
This is a hybrid reputation based blockchain solution.
A global blockchain and multiple regional blockchains are created which communicate with eachother.
Nodes can join either of the chains. The proposed solution can be read in more detail in the paper.

https://github.com/Ammar-Amjad/ReputationBlockchain/blob/main/Final_Report_Group17.pdf
