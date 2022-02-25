import { Module } from '@nestjs/common';
import { TypeormController } from './typeorm.controller';
import { TypeormService } from './typeorm.service';
import { TypeOrmModule } from '@nestjs/typeorm';
import { PostModule } from './post/post.module';
import { CommentModule } from './comment/comment.module';

@Module({
  imports: [
    TypeOrmModule.forRoot({
      type: 'postgres',
      username: 'test',
      password: 'test',
      database: 'test',
      port: 5433,
      synchronize: true,
      autoLoadEntities: true,
      logging: true,
    }),
    PostModule,
    CommentModule,
  ],
  controllers: [TypeormController],
  providers: [TypeormService],
})
export class TypeormModule {}
