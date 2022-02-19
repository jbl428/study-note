import { Test, TestingModule } from '@nestjs/testing';
import { getRepositoryToken } from '@nestjs/typeorm';
import { EntityRepository } from '@mikro-orm/postgresql';
import { Post } from '../src/post/post.entity';
import { Comment } from '../src/comment/comment.entity';
import { MikroORM } from '@mikro-orm/core';
import { MikroOrmModule } from '@mikro-orm/nestjs';
import config from '../src/mikro-orm.config';
import { PostModule } from '../src/post/post.module';
import { CommentModule } from '../src/comment/comment.module';

describe('MikroORM', () => {
  let postEntityRepository: EntityRepository<Post>;
  let commentEntityRepository: EntityRepository<Comment>;
  let orm: MikroORM;

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [
        MikroOrmModule.forRoot({
          ...config,
          allowGlobalContext: true,
        }),
        PostModule,
        CommentModule,
      ],
    }).compile();

    postEntityRepository = moduleFixture.get(getRepositoryToken(Post));
    commentEntityRepository = moduleFixture.get(getRepositoryToken(Comment));
    orm = moduleFixture.get(MikroORM);
  });

  afterAll(() => orm.close());

  it('정상적으로 저장한다', async () => {
    // given
    const post = new Post();
    post.name = 'name';
    post.content = 'content';

    // when
    await postEntityRepository.persistAndFlush(post);

    const result = await postEntityRepository
      .createQueryBuilder('post')
      .where({ id: post.id })
      .getSingleResult();

    expect(result).toBe(post);
  });
});
