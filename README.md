# TRY AGAin - your default retry with backoff in Erlang.

### Default timeout curve in a configuration of 5 attempts with a total duration of 5000 milliseconds:
```mermaid
---
config:
    xyChart:
        width: 500
        height: 400
    themeVariables:
        xyChart:
            backgroundColor: "#adb5bd"
---
xychart-beta

    title "Timeout curve with a factor of 0.66 (default)"

    x-axis "Attempts" [1, 2, 3, 4, 5]
    y-axis "Timeout" 0 --> 4000
    line [369, 559, 847, 1283, 1943]
```

### Aggressive timeout curve in a configuration of 5 attempts with a total duration of 5000 milliseconds:

```mermaid
---
config:
    xyChart:
        width: 500
        height: 400
    themeVariables:
        xyChart:
            backgroundColor: "#adb5bd"
---
xychart-beta

    title "Timeout curve with a factor of 0.33 (aggressive)"

    x-axis "Attempts" [1, 2, 3, 4, 5]
    y-axis "Timeout" 0 --> 4000
    line [40, 121, 366, 1110, 3363]
```
