# Loclastack 을 활용한 통합테스트

이 글은 AWS 서비스를 사용하는 로직의 통합테스트를 위해 `localstack` 을 도입하는 과정을 담고 있습니다.

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
