import { Module } from '@nestjs/common';
import { TypeormController } from './typeorm.controller';
import { TypeormService } from './typeorm.service';

@Module({
  imports: [],
  controllers: [TypeormController],
  providers: [TypeormService],
})
export class TypeormModule {}
