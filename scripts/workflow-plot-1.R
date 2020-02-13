# A minimal plot
library(DiagrammeR)

DiagrammeR("
  graph TD;
    
    A[139 Species] -- NCBI Taxonomy --> B[115 Species];
    B -- NCBI GenBank --> C[107 Species];
    C -- Present EID2 --> D[49 Vector Species];
    C -- Absence EID2 --> E[58 Non-Vector Species]
    
  ")