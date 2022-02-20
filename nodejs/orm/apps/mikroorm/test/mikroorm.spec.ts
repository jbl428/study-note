import { Test, TestingModule } from '@nestjs/testing';
import { getRepositoryToken } from '@nestjs/typeorm';
import { EntityRepository } from '@mikro-orm/postgresql';
import { Post } from '../src/post/post.entity';
import { MikroORM } from '@mikro-orm/core';
import { MikroOrmModule } from '@mikro-orm/nestjs';
import config from '../src/mikro-orm.config';
import { PostModule } from '../src/post/post.module';
import { CommentModule } from '../src/comment/comment.module';
import { PostFactory } from '../src/factory/post-factory';
import { CommentFactory } from '../src/factory/comment-factory';
import { convert, LocalDateTime } from '@js-joda/core';
import { Comment } from '../src/comment/comment.entity';

describe('MikroORM', () => {
  let postEntityRepository: EntityRepository<Post>;
  let commentEntityRepository: EntityRepository<Comment>;
  let orm: MikroORM;
  let postFactory: PostFactory;
  let commentFactory: CommentFactory;

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [
        MikroOrmModule.forRoot({
          ...config,
          allowGlobalContext: true,
          debug: true,
        }),
        PostModule,
        CommentModule,
      ],
    }).compile();

    orm = moduleFixture.get(MikroORM);
    await orm.getSchemaGenerator().refreshDatabase();
    postFactory = new PostFactory(orm.em);
    commentFactory = new CommentFactory(orm.em);
    postEntityRepository = moduleFixture.get(getRepositoryToken(Post));
    commentEntityRepository = moduleFixture.get(getRepositoryToken(Comment));
  });

  afterAll(() => orm.close());

  beforeEach(() =>
    Promise.all([
      postEntityRepository.createQueryBuilder().truncate(),
      commentEntityRepository.createQueryBuilder().truncate(),
    ]),
  );

  it('정상적으로 저장한다', async () => {
    // given
    const post = postFactory.makeOne();
    post.comments.add(...commentFactory.make(5));

    // when
    await postEntityRepository.persistAndFlush(post);

    // then
    const result = await postEntityRepository
      .createQueryBuilder('post')
      .where({ id: post.id })
      .getSingleResult();
    expect(result).toBe(post);
  });

  describe('LocalDateTime', () => {
    it('filterQuery 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      const createdAt = LocalDateTime.of(2022, 2, 1);
      const post = postFactory.makeOne({ createdAt });
      await postEntityRepository.persistAndFlush(post);

      // when
      const result = await postEntityRepository
        .createQueryBuilder('post')
        .where({ createdAt: { $gte: createdAt } })
        .getSingleResult();

      // then
      expect(result).toBe(post);
    });

    it('string cond 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      LocalDateTime.prototype.toJSON = function () {
        return convert(this).toDate().toISOString();
      };
      const createdAt = LocalDateTime.of(2022, 2, 1);
      const post = postFactory.makeOne({ createdAt });
      await postEntityRepository.persistAndFlush(post);

      // when
      const result = await postEntityRepository
        .createQueryBuilder('post')
        .where('post.created_at = ?', [createdAt])
        .getSingleResult();

      // then
      expect(result?.id).toBe(post.id);
    });

    it('alias 있는 queryFilter 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      const createdAt = LocalDateTime.of(2022, 2, 1);
      const post = postFactory.makeOne({ createdAt });
      await postEntityRepository.persistAndFlush(post);

      // when
      const result = await postEntityRepository
        .createQueryBuilder('post')
        .where({ 'post.createdAt': { $gte: convert(createdAt).toDate() } })
        .getSingleResult();

      // then
      expect(result?.id).toBe(post.id);
    });
  });
});
