import { Module } from '@nestjs/common';
import { MikroormController } from './mikroorm.controller';
import { MikroormService } from './mikroorm.service';
import { MikroOrmModule } from '@mikro-orm/nestjs';

@Module({
  imports: [
    MikroOrmModule.forRoot({
      type: 'postgresql',
      user: 'test',
      password: 'test',
      dbName: 'test',
    }),
  ],
  controllers: [MikroormController],
  providers: [MikroormService],
})
export class MikroormModule {}
