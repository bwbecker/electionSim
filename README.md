# electionSim
A program to simulate Canadian elections using FPTP, AV, MMP, STV, and RU-PR.

##Thoughts for an Overhaul
* The Design class should focus strictly on the riding design.  Keep 
candidates out of it.
* Candidates:
    * Make CandidateList a separate class from Design. 
    * Make explicit that there are three states for a candidate: 
    Elected, NotElected, and NotYetDecided.
    * Perhaps make a case class with a data structure (set?, map?) for each of
     the three states
