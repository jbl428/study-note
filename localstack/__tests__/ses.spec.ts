import { GenericContainer, Wait } from 'testcontainers';
import { SendEmailCommand, SESClient, VerifyEmailAddressCommand } from '@aws-sdk/client-ses';

describe('AWS SES', () => {
  let localstackPort: number;

  beforeAll(async () => {
    const container = await new GenericContainer('localstack/localstack')
    .withExposedPorts(4566)
    .withEnv('SERVICES', 'ses')
    .withWaitStrategy(
      Wait.forLogMessage('Execution of "preload_services"')
    )
    .start();

    localstackPort = container.getMappedPort(4566)
  })

  it('verify and send mail', async () => {
    const verifiedEmail = 'test@email.com'
    const client = new SESClient({
      region: 'local',
      endpoint: `http://localhost:${localstackPort}`,
      credentials: {
        accessKeyId: 'test',
        secretAccessKey: 'test',
      },
    });
    const command = new VerifyEmailAddressCommand({
      EmailAddress: verifiedEmail
    });
    await client.send(command);

    const sendEmailCommand = new SendEmailCommand({
      Source: verifiedEmail,
      Destination: {
        ToAddresses: ['to@test.com']
      },
      Message: {
        Subject: {
          Data: 'subject'
        },
        Body: {
          Html: {
            Data: '<strong>text</strong>'
          }
        }
      }

    });
    const response = await client.send(sendEmailCommand );

    expect(response.$metadata.httpStatusCode).toBe(200)
  })
})