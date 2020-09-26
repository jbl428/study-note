# MySQL

## Join을 활용한 업데이트

MySQL은 update 구문의 set 위치에 서브쿼리를 넣는것을 지원하지 않지만 join을 활용하여 해결할 수 있는 경우가 있다.
아래와 같은 테이블과 데이터가 존재한다고 가정하자.

```text
mysql> desc address;
+--------------+--------------+------+-----+---------+-------+
| Field        | Type         | Null | Key | Default | Extra |
+--------------+--------------+------+-----+---------+-------+
| uid          | char(5)      | YES  |     | NULL    |       |
| name         | varchar(256) | YES  |     | NULL    |       |
| code         | int          | YES  |     | NULL    |       |
| updated_date | date         | YES  |     | NULL    |       |
+--------------+--------------+------+-----+---------+-------+

mysql> select * from address;
+-------+------+------+--------------+
| uid   | name | code | updated_date |
+-------+------+------+--------------+
| 12345 | Foo  |   10 | 2020-08-01   |
| 54321 | Bar  |   20 | 2020-08-01   |
| 12345 | Foo  |    0 | 2020-09-01   |
| 54321 | Bar  |    0 | 2020-09-01   |
+-------+------+------+--------------+
```

9월 데이터의 code를 8월 1일 데이터의 값으로 설정하고 싶다고하자.
uid가 name이 같은것을 8월 데이터에서 찾아서 설정해야한다. 즉 원하는 결과는 아래와 같다.

```text
+-------+------+------+--------------+
| uid   | name | code | updated_date |
+-------+------+------+--------------+
| 12345 | Foo  |   10 | 2020-08-01   |
| 54321 | Bar  |   20 | 2020-08-01   |
| 12345 | Foo  |   10 | 2020-09-01   |
| 54321 | Bar  |   20 | 2020-09-01   |
+-------+------+------+--------------+
```

물론 위 상황에서는 update구문을 2번 사용하면 해결되나 데이터가 엄청나게 많은 경우에는 힘들다.
코드를 만들어서 해도 되지만 update구문에 join을 이용하면 하나의 쿼리만 사용해서 문제를 해결할 수 있다.

```sql
UPDATE address AS a
    INNER JOIN (SELECT * FROM address WHERE updated_date = '2020-08-01') AS b
        ON a.uid = b.uid AND a.name = b.name
SET a.code = b.code
WHERE a.updated_date = '2020-09-01'
)
```

8월 데이터와 9월 데이터를 uid와 name을 이용해서 inner join한 결과에서 8월 데이터의 code를 9월 데이터의 code로 변경하게 된다.

## 테스트

위 내용을 테스트 해볼 수 있는 환경을 쿠버네티스 배포파일을 만들어 두었으며 아래명령어로 실행한다.

```bash
> kubectl apply -f deployment.yaml
```

정상적으로 파드가 올라왔는지 확인한다.

```bash
> kubectl get pods
NAME                              READY   STATUS    RESTARTS   AGE
mysql-deployment-cb6fcc8c-jkszv   1/1     Running   2          19m
```

아래명령어로 컨테이너에 접속 후 mysql에 접속해서 테스트 할 수 있다. `파드 이름` 부분은 위 명령어를 통해 나온 `NAME`으로 치환해서 수행한다.

```bash
> kubectl exec -it "파드 이름" bash
root@mysql-deployment-cb6fcc8c-jkszv:/ mysql -p
Enter password: # 비밀번호는 password
mysql> use test; # test 데이터베이스에 address 테이블이 존재한다
```

테스트가 끝나면 파드접속을 마친 후 아래 명령어를 수행하면 파드를 삭제한다.

```bash
> kubectl delete -f deployment.yaml
```
