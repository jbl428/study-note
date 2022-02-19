import { Module } from '@nestjs/common';
import { MikroormController } from './mikroorm.controller';
import { MikroormService } from './mikroorm.service';
import { MikroOrmModule } from '@mikro-orm/nestjs';
import { PostModule } from './post/post.module';
import { CommentModule } from './comment/comment.module';
import config from './mikro-orm.config';

@Module({
  imports: [MikroOrmModule.forRoot(config), PostModule, CommentModule],
  controllers: [MikroormController],
  providers: [MikroormService],
})
export class MikroormModule {}
