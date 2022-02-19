import { Module } from '@nestjs/common';
import { MikroormController } from './mikroorm.controller';
import { MikroormService } from './mikroorm.service';
import { MikroOrmModule } from '@mikro-orm/nestjs';
import { PostModule } from './post/post.module';
import { TsMorphMetadataProvider } from '@mikro-orm/reflection';
import { CommentModule } from './comment/comment.module';

@Module({
  imports: [
    MikroOrmModule.forRoot({
      type: 'postgresql',
      user: 'test',
      password: 'test',
      dbName: 'test',
      metadataProvider: TsMorphMetadataProvider,
    }),
    PostModule,
    CommentModule,
  ],
  controllers: [MikroormController],
  providers: [MikroormService],
})
export class MikroormModule {}
