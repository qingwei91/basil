## To run

```
sbt benchmark/jmh:run -p size=1,10,100 ADTBenchmark

sbt benchmark/jmh:run -i 10 -wi 3 -f1 -t1 ADTBenchmark
```
