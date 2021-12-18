
# The results are in

Let's face it, GPXmagic struggles when faced with many track points. 

This is, indirectly, a consequence of making it a "web app", so it all runs inside a browser.
I'd also not been overly concerned about making it efficient in any terms, more on making it work.

For 2.8, I decided to take it all apart, weigh all the bits, polish them, drill holes in them, look 
at upgrades, then carefully reassemble and hope that it's lighter, quicker, more nimble.

That took a while, and while I felt that it was better, there's no arguing with data. We've just 
out of the wind tunnel and the numbers tell the tale, for better or worse.

## Load times (ms)

| Points | 98 | 885 | 1854 | 7883 | 23176 | 57784 |
|---|---:|---:|---:|---:|---:|---:|
| v2.7 | 356 | 571 | 626 | 1574 | 5883 | 21965 |
| v2.8 | 407 | 508 | 733 | 1313 | 2583 | 11994 |
| Change | +14% | -11% | +17% | -17% | -56% | -45% | 

Something of a mixed message. Good news, it pays off for large tracks, which is where we had the problems.
For smaller tracks, it might be faster, or slower, but not by so much that you'd necessarily notice (these
 numbers are in milliseconds) .


