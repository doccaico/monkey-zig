# Monkey

A Zig/Ziglang version [monkey](https://monkeylang.org/) language interpreter. From [Write An Interpreter In Go](https://interpreterbook.com/).

## Test
```
# do all tests (on Windows)

$ zig test src\main.zig

# 単体テスト

$ zig test src\Evaluator.zig --test-filter TestClosures (通らないが...)
```

## Memo
```
Evaluation - 3.10 - Functions & Function Calls - TestClosures が通らないので頓挫 2024/03/30
```
