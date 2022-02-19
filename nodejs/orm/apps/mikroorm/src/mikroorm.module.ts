import { Module } from '@nestjs/common';
import { MikroormController } from './mikroorm.controller';
import { MikroormService } from './mikroorm.service';

@Module({
  imports: [],
  controllers: [MikroormController],
  providers: [MikroormService],
})
export class MikroormModule {}
