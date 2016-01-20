Data Structures ---- R
========================================================


R's base data structures can be organised by their dimensionality (1d, 2d, or nd) and whether they're homogeneous (all contents must be of the same type) or heterogeneous (the contents can be of different types). This gives rise to the five data types most often used in data analysis: 

|    | Homogeneous   | Heterogeneous |
|----|---------------|---------------|
| 1d | Atomic vector | List          |
| 2d | Matrix        | Data frame    |
| nd | Array         |               |

- ("Advanced R", Wickham 2014) http://adv-r.had.co.nz/


Data Structures ---- R ---- Vectors
========================================================
- 1 dimensional data structures 
- Must all be one type i.e. "homogeneous"
- Formed by the "c()" operator
e.g.


```r
data <- c(1,2,3,4,10)
data
```

```
[1]  1  2  3  4 10
```

```r
data <- c("Help", "Me", "I'm", "Not", "Good", "At", "Examples")
data
```

```
[1] "Help"     "Me"       "I'm"      "Not"      "Good"     "At"      
[7] "Examples"
```
Data Structures ---- R ---- Vectors
========================================================
What happens with mixed types?

```r
data<-c("r", "u", "da", 1)
data
```

```
[1] "r"  "u"  "da" "1" 
```

Data Structures ---- R ---- Lists
========================================================
- 1 dimensional data structures 
- Can be mixed types i.e. "heterogeneous"
- Formed by the "list()" function

```r
data <- list(1,2,3)
data
```

```
[[1]]
[1] 1

[[2]]
[1] 2

[[3]]
[1] 3
```
Data Structures ---- R ---- Lists
========================================================
- Mixed Types?

```r
data <- list("r", "u", "da", 1)
data
```

```
[[1]]
[1] "r"

[[2]]
[1] "u"

[[3]]
[1] "da"

[[4]]
[1] 1
```
Data Structures ---- R ---- data.frames
========================================================
- What are lists that contain vectors? Data Frames.
- R structures data files e.g. .csv, .txt, etc... as data frames
- These come with a bunch of useful functionality
- You will see these alot.
