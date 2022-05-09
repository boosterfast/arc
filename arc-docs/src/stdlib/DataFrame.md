# DataFrames

DataFrames store data in rows and columns, where cells are scalars. The idea in Arc-Lang is to build up DataFrames incrementally.

```arc-lang
extern type DataFrame;
```

```arc-lang
extern def df(): DataFrame;
extern def append[T: BaseRow](DataFrame, T);
extern def head(DataFrame, i32): DataFrame;
extern def tail(DataFrame, i32): DataFrame;
extern def take(DataFrame, i32): DataFrame;
extern def join(DataFrame, DataFrame, [String], [String]): DataFrame;
```

```arc-lang
val df = DataFrame(
  {name: "John", age: 30, city: "New York"},
  {name: "Jane", age: 22, city: "San Francisco"},
  {name: "Joe",  age: 49, city: "Boston"},
  {name: "Jill", age: 35, city: "New York"},
);
df.append({name: "Mary", age: 12});

val df = DataFrame({
  "name": ["John", "Jane", "Joe", "Jill"],
   "age": [30, 22, 49, 35],
  "city": ["New York", "San Francisco", "Boston", "New York"]}
);
df.append({"name": "Mary", "age": 12});
extern type Series;
extern def df(data: Array[Series]?): DataFrame;
extern def series[T: Scalar](name: String, data: Array[T]);
extern def Series[T: Scalar](name: String, values: Array[T]): Series;
extern def __index__(df: DataFrame, col: i32): Array[String];

extern def ds(): DataSet;
extern def append[T](DataSet[T], T);
extern def head[T](DataSet[T], i32): DataSet[T];
extern def tail[T](DataSet[T], i32): DataSet[T];
extern def take[T](DataSet[T], i32): DataSet[T];
extern def join_df[A, B](DataSet[A], DataSet[B], [String], [String]): DataSet[A ++ B];
```


A series is a named one-dimensional array of data representing a column in a DataFrame.

```arc-lang
val obj = {"name": "John", "age": 30, "city": "New York"};

val name = obj["name"];
```


