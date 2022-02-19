import { NestFactory } from '@nestjs/core';
import { MikroormModule } from './mikroorm.module';

async function bootstrap() {
  const app = await NestFactory.create(MikroormModule);
  await app.listen(3000);
}
bootstrap();
