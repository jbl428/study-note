import { Module } from '@nestjs/common';
import { MikroOrmModule } from '@mikro-orm/nestjs';
import { Comment } from './comment.entity';

@Module({
  imports: [MikroOrmModule.forFeature([Comment])],
})
export class CommentModule {}
