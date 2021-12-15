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

## Localstack 의 단점

이처럼 localstack 을 이용해면 통합테스트를 하는데 많은 도움을 받을 수 있지만 여러 불편한 점과 이슈가 있었습니다.

1. SES v2 를 지원하지 않음

2. 병렬 테스트 실행의 어려움

3. CI 에서 localstack 이 완전히 실행된 이후에 테스트 실행해야 함

## Localstack

[localstack](https://localstack.cloud/) 은 aws 인프라 시뮬레이터로 **Lambda, S3, Dynamodb, Kinesis, SQS, SNS** 같은 서비스를 로컬 환경에 올릴 수 있습니다.
aws 서비스를 사용하기 위해 보통 프로그래밍 언어별로 제공하는 sdk 를 사용하는데 사실 sdk 는 요청이 들어오면 값을 가공해 지정된 endpoint 로 http 요청을 수행해주는 client 라 할 수 있습니다.
따라서 localstack 은 하나의 http 서버이며 aws api 스펙에 맞는 요청이 들어오면 요청에 대한 실제 인프라 작업은 하지 않지만 요청값에 따라 성공 또는 실패의 결과를 내려줍니다.
이를 통합테스트에 활용하면 다음과 같은 이점을 얻을 수 있습니다.

- 실제 aws 리소스를 사용하지 않으므로 비용이 들지않는다.
- 로컬이나 CI 에서 **aws secret key** 같은 사전에 필요한 정보를 주입받는 과정이 필요없다.

현재 localstack 은 docker image 로 제공하며 기본값으로 4566 포트를 사용하고 있습니다.
이번에 메일 발송과 파일 저장 모듈을 만들면서 테스트 방법에 대한 고민을 하게되었고 `localstack` 을 사용하기로 결정하였습니다.

## 테스트 방법

통합테스트 실행하기 위해서는 사전에 `localstack` 컨테이너가 올라와 있어야 하고 보통 다음 방법 중 하나를 사용합니다.

- Docker compose
- Testcontainers

전자는 테스트 코드와 독립적으로 컨테이너를 실행하는 방법이고 후자는 컨테이너의 실행 및 종료를 라이브러리를 활용해 수행합니다.
각 방법마다 장단점이 존재하므로 상황에 따라 적절한 방법을 사용하는게 좋습니다.

> 이 글을 작성한 시점에서는 아직 localstack 이 SES v2 API 를 지원하지 않아 v1 기준으로 작성하였습니다.
> [관련이슈](https://github.com/localstack/localstack/issues/3179)

### Docker compose 로 실행하기

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
$ docker-compose up -d

$ docker-compose logs # 일정 시간 기다린 후 수행

localstack_1  | Starting edge router (https port 4566)...
localstack_1  | 2021-12-12 06:42:46,346 INFO success: infra entered RUNNING state, process has stayed up for > than 1 seconds (startsecs)
localstack_1  | Ready.
localstack_1  | [2021-12-12 06:42:46 +0000] [21] [INFO] Running on https://0.0.0.0:4566 (CTRL + C to quit)
localstack_1  | 2021-12-12T06:42:46.345:INFO:hypercorn.error: Running on https://0.0.0.0:4566 (CTRL + C to quit)
localstack_1  | 2021-12-12T06:42:46.476:INFO:bootstrap.py: Execution of "start_runtime_components" took 608.23ms
localstack_1  | /usr/local/bin/docker-entrypoint.sh: running /docker-entrypoint-initaws.d/init.sh
localstack_1  | Init localstack
localstack_1  | make_bucket: test-bucket
localstack_1  |
localstack_1  | 2021-12-12T06:42:51.171:INFO:localstack.services.motoserver: starting moto server on http://0.0.0.0:55701
localstack_1  | 2021-12-12T06:42:51.171:INFO:localstack.services.infra: Starting mock S3 service on http port 4566 ...
localstack_1  | 2021-12-12T06:42:51.510:INFO:localstack.services.infra: Starting mock SES service on http port 4566 ...

$ curl http://localhost:4566/health
{"status": "running"}
```

아래와 같은 간단한 테스트 코드를 작성한 후 실행하면 성공하는 것을 확인할 수 있습니다.

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

### Testcontainers 로 실행하기

[Testcontainers](https://www.testcontainers.org/) 는 이름 그대로 테스트를 위한 컨테이너를 실행해주는 라이브러리입니다.
코드로 컨테이너를 띄우고 종료할 수 있게 해주며 동시에 여러 컨테이너를 올리는 경우 자동으로 임의의 포트로 expose 해주어 각 테스트 케이스간에 서로 다른 컨테이너를 사용하면 독립성을 지킬 수 있습니다.
또한 테스트 종료 이후 직접 컨테이너를 종료로직을 넣지 않아도 일정기간 활동이 없으면 자동으로 종료해주는 기능이 있습니다.

> 정확이 말하면 라이브러리가 종료하는게 아니고 컨테이너 실행 시 `testcontainers/ryuk` 라는 이름의 컨테이너가 같이 실행되는데 이 컨테이너가 종료를 담당합니다.

**Java, Go, Scala, Node** 등 여러 프로그래밍 언어를 지원하며 그 중 Java 가 가장 많은 star 를 가지고 있습니다.
Java 라이브러리의 경우 localstack 용 모듈이 따로 있어서 편리하게 이용할 수 있지만 node 그렇지 않아서 직접 설정해야 하는 부분이 존재합니다.
먼저 아래 명령어로 라이브러리를 설치합니다.

```bash
$ npm i -D testcontainers
```

이제 아래 테스트 코드를 작성합니다.

```typescript
import { GenericContainer, Wait } from "testcontainers";
import {
  SendEmailCommand,
  SESClient,
  VerifyEmailAddressCommand,
} from "@aws-sdk/client-ses";

describe("SES 테스트", () => {
  let localstackPort: number;
  let container: GenericContainer;
  const verifiedEmail = "test@email.com";
  const client = new SESClient({
    region: "local",
    endpoint: `http://localhost:${localstackPort}`,
    credentials: {
      accessKeyId: "test",
      secretAccessKey: "test",
    },
  });

  beforeAll(async () => {
    container = await new GenericContainer("localstack/localstack")
      .withExposedPorts(4566)
      .withEnv("SERVICES", "ses")
      .withWaitStrategy(Wait.forLogMessage('Execution of "preload_services"'))
      .start();

    localstackPort = container.getMappedPort(4566);
  });

  beforeAll(async () => {
    const command = new VerifyEmailAddressCommand({
      EmailAddress: verifiedEmail,
    });
    await client.send(command);
  });

  afterAll(() => container.close());

  it("등록한 전송자 주소로 메일 전송요청 시 성공응답을 받는다", async () => {
    // given
    const sendEmailCommand = new SendEmailCommand({
      Source: verifiedEmail,
      Destination: {
        ToAddresses: ["to@test.com"],
      },
      Message: {
        Subject: {
          Data: "subject",
        },
        Body: {
          Html: {
            Data: "<strong>text</strong>",
          },
        },
      },
    });

    // when
    const response = await client.send(sendEmailCommand);

    // then
    expect(response.$metadata.httpStatusCode).toBe(200);
    expect(response.MessageId).toBeTruthy();
  });
});
```

docker compose 를 사용한 코드와 달리 `beforeAll` 부분에 컨테이너를 만들고 이메일을 등록하는 로직이 추가되었고 `afterAll` 에 컨테이너를 종료하는 로직이 추가되었습니다.

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
