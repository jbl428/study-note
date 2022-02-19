import { Module } from '@nestjs/common';
import { TypeormController } from './typeorm.controller';
import { TypeormService } from './typeorm.service';
import { TypeOrmModule } from '@nestjs/typeorm';

@Module({
  imports: [
    TypeOrmModule.forRoot({
      type: 'postgres',
      username: 'test',
      password: 'test',
      database: 'test',
    }),
  ],
  controllers: [TypeormController],
  providers: [TypeormService],
})
export class TypeormModule {}
