import { NestFactory } from '@nestjs/core';
import { TypeormModule } from './typeorm.module';

async function bootstrap() {
  const app = await NestFactory.create(TypeormModule);
  await app.listen(3000);
}
bootstrap();
