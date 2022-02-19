import { Module } from '@nestjs/common';
import { MikroOrmModule } from '@mikro-orm/nestjs';
import { Post } from './post.entity';

@Module({
  imports: [MikroOrmModule.forFeature([Post])],
})
export class PostModule {}
