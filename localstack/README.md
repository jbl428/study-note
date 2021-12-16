# Loclastack 을 활용한 통합테스트

이 글은 AWS 서비스를 사용하는 로직의 통합테스트를 위해 `localstack` 을 도입하는 과정을 담고 있습니다.

## 통합테스트

유닛테스트 만으로는 소프트웨어가 정상적으로 동작함을 보장할 수 없습니다.
그렇다고 실제 AWS 리소스를 활용하는 경우 다음과 같은 문제가 발생합니다.

1. 비용

테스트가 실행되는 상황은 보통 아래와 같습니다.

- 개발자 각자 로컬에서 실행
- CI / CD

하루에도 수십번의 테스트 실행이 이루어지는 상황에서 매번 AWS 리소스를 사용하다 보면 무시할 수 없는 비용의 문제가 발생할 수 있습니다.

2. secret 관리

실제 AWS 리소스에 접근하려면 `secret access key` 같은 접속정보가 필요합니다.
이를 위해 각 개발자 로컬에 해당 정보를 제공해야 하며 그만큼 유출 가능성이 높아집니다.

3. 독립성을 보장받기 힘듬

올바른 테스트를 위해서는 테스트 케이스간 독립성이 중요합니다.
즉 테스트 순서에 따라 결과가 달라지거나 어떤 테스트의 결과가 다른 테스트에 영향을 끼쳐선 안됩니다.
하지만 하나의 AWS 리소스를 사용하는 경우 테스트의 병렬실행 시 문제가 발생할 수 있습니다.
예로들면 SQS 에 메시지를 넣는 테스트 코드와 빼내는 테스트 코드가 있다면 두 테스트가 동시에 실행되었을 때 타이밍에 따라 빼낸 메시지가 기대했던 값과 달라 때때로 테스트가 실패할 수 있습니다.
이를 해결하기 위해 각 테스트간 다른 AWS 리소스를 사용하도록 할 수 있지만 이와 같은 케이스마다 매번 리소스를 무한정 늘릴 수 없다는 한계가 있습니다.

## Testcontainers

테스트를 위한 컨테이너를 띄우는 방법으로 [Testcontainers](https://www.testcontainers.org/) 가 있습니다.
이 라이브러리는 이름 그대로 테스트를 위한 컨테이너를 실행해줍니다.
`jest` 의 `beforeAll` 과 같은 테스트 실행 전 수행되는 곳에 라이브러리를 사용해 컨테이너를 올립니다.

```ts
let localstackPort: number;
let container: GenericContainer;

// 컨테이너 실행
beforeAll(async () => {
  container = await new GenericContainer("localstack/localstack")
    .withExposedPorts(4566)
    .withEnv("SERVICES", "ses")
    .withWaitStrategy(Wait.forLogMessage('Execution of "preload_services"'))
    .start();

  localstackPort = container.getMappedPort(4566);
});

// 컨테이너 종료
afterAll(() => container.close());
```

위처럼 컨테이너의 실행과 종료를 코드로 관리할 수 있기에 각 테스트간 독립적인 환경을 제공할 수 있습니다.
따라서 병렬실행이 가능하다는 장점이 있습니다.
하지만 저희는 testcontainer 대신 docerk-compose 를 활용하기로 결정했고 그 이유는 다음과 같습니다.

- 단건 테스트의 속도가 느려짐

테스트 실행여부와 관계없이 항상 컨테이너를 실행해둘 수 있는 docker 방식과 달리 테스트를 실행할 때 컨테이너를 실행하고 테스트 종료 후 마무리 하는 과정이 필요합니다.
이로인해 단건 테스트의 속도가 docker 에 비해 많이 느려집니다.
로컬 환경에서 개발 시 전체 테스트 보다 단건 테스트를 더 많이 수행하게 되므로 생산성 측면에서 오히려 손해라고 판단하였습니다.

- 생각보다 병렬실행의 이점을 얻기 힘들다.

testcontainer 를 통해 얻을 수 있는 독립성을 활용해 여러 테스트를 병렬로 실행할 수 있으나 대량의 컨테이터를 올리고 내리는데 드는 부하를 무시할 수 없기에 큰 속도향상을 기대하기 힘든 상황이었습니다.

## Localstack 의 단점

이처럼 localstack 을 이용해면 통합테스트를 하는데 많은 도움을 받을 수 있지만 여러 불편한 점과 이슈가 있었습니다.

1. SES v2 를 지원하지 않음

2. 병렬 테스트 실행의 어려움

3. CI 에서 localstack 이 완전히 실행된 이후에 테스트 실행해야 함

### Docker compose 로 실행하기

위와 같은 이유로 저희는 docker 로 단일 localstack 을 실행하는 것으로 결정하였고 이제 그 과정을 설명하려고 합니다.
`localstack` 는 컨테이너 설정을 위해 환경변수를 제공하는데 `docker compose` 를 활용하면 필요한 값들의 명세(yaml)를 관리할 수 있습니다.
[문서](https://hub.docker.com/r/localstack/localstack) 에 모든 환경변수에 대한 설명이 있으며 그 중 몇가지를 살펴보면

- EDGE_PORT: API 서버가 사용할 포트번호를 넣습니다. 기본값은 **4566**
- SERVICE: 시뮬레이트를 원하는 서비스명을 `,` 를 구분자로 하여 넣습니다.
- DEFAULT_REGION: 기본으로 사용할 리전을 넣습니다.

ses 를 사용하려면 발송인 이메일 주소가 사전에 등록되어 있어야 하고 s3 는 버킷을 만들어야 합니다.
localstack 도 실제 메일을 보내거나 버킷을 만들지는 않지만 등록 api 와 버킷 생성 api 를 사전에 수행하지 않은경우 에러 응답을 내려줍니다.
sdk 를 활용해서 테스트 코드 실행 전 등록 api 를 수행해도 되지만 *초기화 스크립트*를 활용하면 편리하게 사전작업을 할 수 있습니다.
`localstack` 은 컨테이너 실행 시 `/docker-entrypoint-initaws.d` 경로 밑의 스크립트 파일을 읽어 실행합니다.
이를 활용해 먼저 아래 내용의 스크립트를 `localstack-init` 디렉토리 밑에 생성합니다.

- init.sh

```shell
#!/bin/sh
echo "Init localstack"
awslocal s3 mb s3://test-bucket
awslocal ses verify-email-identity --email-address test@email.com
```

`awslocal` 명령어는 `localstack` api 서버에 요청을 하는것을 제외하면 [aws cli](https://aws.amazon.com/ko/cli/) 와 사용법이 동일합니다.
내용을 보면 `test-bucket` 이라는 이름의 버킷을 만들고 `test@email.com` 이메일 주소를 인증하는 작업을 수행하게 됩니다.
이제 docker compose 파일을 아래와 같이 생성합니다.

- docker-compose.yml

```yaml
version: "3.9"

services:
  localstack:
    image: localstack/localstack
    ports:
      - "4566:4566"
    environment:
      - SERVICES=ses,s3
    volumes:
      - "./localstack-init:/docker-entrypoint-initaws.d"
```

이제 아래 명령어를 수행하면 컨테이너가 실행되며 로그를 확인하면 초기화 스크립트를 실행했고 s3 와 ses 서비스가 4566 포트에 올라온 것을 확인할 수 있습니다.

```shell
$ docker-compose up
```

![image](./localstack-start.png)

이제 아래와 같은 간단한 테스트 코드를 작성한 후 실행하면 성공하는 것을 확인할 수 있습니다.

```typescript
import { SendEmailCommand, SESClient } from "@aws-sdk/client-ses";

describe("SES 테스트", () => {
  const client = new SESClient({
    region: "local", // 임의의 region 을 주어도 정상동작합니다.
    credentials: { secretAccessKey: "test", accessKeyId: "test" }, // 계정정보도 임의의 값을 주어도 정상동작합니다.
    endpoint: "http://localhost:4566", // localstack 주소로 변경합니다.
  });

  it("등록한 전송자 주소로 메일 전송요청 시 성공응답을 받는다", async () => {
    // given
    const from = "test@email.com";
    const command = new SendEmailCommand({
      Source: from,
      Destination: {
        ToAddresses: ["foo@domain.com"],
      },
      Message: {
        Subject: {
          Data: "메일 제목",
          Charset: "UTF-8",
        },
        Body: {
          Html: {
            Data: "메일 내용",
            Charset: "UTF-8",
          },
        },
      },
    });

    // when
    const response = await client.send(command);

    // then
    expect(response.$metadata.httpStatusCode).toBe(200);
    expect(response.MessageId).toBeTruthy();
  });

  it("미등록 전송자 주소로 메일 전송요청 시 에러가 발생한다", async () => {
    // given
    const from = "invalid@email.com";
    const command = new SendEmailCommand({
      Source: from,
      Destination: {
        ToAddresses: ["foo@domain.com"],
      },
      Message: {
        Subject: {
          Data: "메일 제목",
          Charset: "UTF-8",
        },
        Body: {
          Html: {
            Data: "메일 내용",
            Charset: "UTF-8",
          },
        },
      },
    });

    // when
    const send = () => client.send(command);

    // then
    await expect(send).rejects.toThrowError("MessageRejected");
  });
});
```

1. AWS 자원을 이용한 통합 테스트 작성의 문제점 (어려운점)
2. 왜 LocalStack을 선택했는지이
   1. aws 를 직접 사용하는건 왜 문제가 되는지
   2. 로컬에서 독립적인 테스트 환경이 왜 필요한 것인지
3. 왜 테스트 컨테이너가 아닌 docker 로 Localstack 실행을 선택했는지
4. Localstack 예제 코드 및 사용법
5. Localstack을 도입하면서 주의해야할 점 (내가 삽질한점)
   1. 이번에 버전 다운그레이드로 해결한 이슈
   2. 컨테이너가 완전히 실행되기를 기다린 후 테스트 실행
6. LocalStack을 도입하면서 얻게된 점 (마치며)
   1. 중간 중간 LocalStack으로 돌린 테스트 로그와 결과물에 대한 캡쳐 이미지
   2. 로컬 스택을 통한 통합 테스트 구조를 그린 이미지
