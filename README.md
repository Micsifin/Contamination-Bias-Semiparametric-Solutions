# Replication Package for Beyond Linearity: Semiparametric Solutions to Contamination Bias with Multi-valued Treatments, Finger and Possebom (2026)

For the replication, you will need to create 3 new folders: `MulTE/`, `Output Images/`, and `Output Tables` in the same directory as the files. The resulting file and folder structure should be the following:

```
Replication Package/
├── README.md                            (this file)
├── Functions.R                          (hosts necessary functions)
├── Regressions.R                        (runs all regressions necessary)
├── tables-figures.R                     (makes tables and figures)
├── Data/                                (hosts all data)
|   └── ...
├── Multe/                               (should be created, should hold regression/MulTE results)
|   └── ...
├── Output Images/                       (should be created, should hold resulting images)
|   └── ...
└── Output Tables/                       (should be created, should hold resulting tables)
|   └── ...
```

To replicate the paper's results, first set the working directory in the beginning of the `Regressions.R` and `tables-figures.R`. Then, run the entire `Regressions.R` code, and then `tables-figures.R`. The corresponding files with the tables and images should be in the correct files afterwards.

## Data availability

All the data used in the paper is available in the `Data/` folder. The data used in this paper is publicly available on each of the replicated articles' replication package.