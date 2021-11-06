# 슬롯 머신

스칼라로 배우는 함수형 프로그래밍에 나온 state 연습문제를 하스켈로 푼 코드

## 문제

사탕 판매기를 구현하라. 입력은 동전을 넣거나 사탕이 나오는 손잡이들 돌리는 행위이다.
작동 규칙은 다음과 같다.

- 잠긴 판매기에 동전을 넣으면, 사탕이 남아 있는 경우 잠김이 풀린다.
- 풀린 판매기의 손잡이를 돌리면 사탕이 나오고 판매기가 잠긴다.
- 잠긴 판매기의 손잡이를 돌리거나 풀린 판매기에 동전을 넣으면 아무 일도 일어나지 않는다.
- 사탕이 없는 판매기는 모든 입력을 무시한다.

simulateMachine 메서드는 입력들의 리스트를 받아서 남은 동전과 사탕 개수를 돌려준다.

```hs
data Input = Coin | Turn

data Machine = Machine {
    locked :: Bool,
    candies :: Int,
    coins :: Int,
}

simulateMachine :: [Input] -> State Machine (Int, Int)
```
