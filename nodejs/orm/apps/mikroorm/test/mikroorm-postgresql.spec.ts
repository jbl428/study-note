import { Test, TestingModule } from '@nestjs/testing';
import { EntityRepository } from '@mikro-orm/postgresql';
import { Post } from '../src/post/post.entity';
import { LoadStrategy, MikroORM } from '@mikro-orm/core';
import { getRepositoryToken, MikroOrmModule } from '@mikro-orm/nestjs';
import config from '../src/mikro-orm.config';
import { PostModule } from '../src/post/post.module';
import { CommentModule } from '../src/comment/comment.module';
import { PostFactory } from '../src/factory/post-factory';
import { CommentFactory } from '../src/factory/comment-factory';
import { convert, LocalDateTime } from '@js-joda/core';
import { Comment } from '../src/comment/comment.entity';
import { PostStatus } from '../src/post/post-status';

describe('MikroORM (postgresql)', () => {
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
    const extra = { foo: 'text', bar: [true, 1] };
    const post = postFactory.makeOne({ extra, someIds: [1, 2, 3] });
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
    it('repository 사용하는 경우 정상적으로 동작한다 ', async () => {
      // given
      const createdAt = LocalDateTime.of(2022, 2, 1);
      const post = postFactory.makeOne({ createdAt });
      post.comments.add(...commentFactory.make(5));
      await postEntityRepository.persistAndFlush(post);
      orm.em.clear();

      // when
      const result = await postEntityRepository.findOneOrFail(
        { createdAt: { $gte: createdAt } },
        { populate: ['comments'] },
      );

      // then
      expect(result.id).toBe(post.id);
      expect(result.comments).toHaveLength(5);
    });

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

  describe('EClass', () => {
    it('filterQuery 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      const status = PostStatus.PRIVATE;
      const post = postFactory.makeOne({ status });
      await postEntityRepository.persistAndFlush(post);

      // when
      const result = await postEntityRepository
        .createQueryBuilder('post')
        .where({ status: { $eq: status } })
        .getSingleResult();

      // then
      expect(result).toBe(post);
    });

    it('string cond 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      const status = PostStatus.PUBLIC;
      const post = postFactory.makeOne({ status });
      await postEntityRepository.persistAndFlush(post);

      // when
      const result = await postEntityRepository
        .createQueryBuilder('post')
        .where('post.status = ?', [status.code])
        .getSingleResult();

      // then
      expect(result?.id).toBe(post.id);
    });

    it('alias 있는 queryFilter 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      const status = PostStatus.PUBLIC;
      const post = postFactory.makeOne({ status });
      await postEntityRepository.persistAndFlush(post);

      // when
      const result = await postEntityRepository
        .createQueryBuilder('post')
        .where({ 'post.status': { $eq: status.code } })
        .getSingleResult();

      // then
      expect(result?.id).toBe(post.id);
    });
  });

  describe('select', () => {
    it('inner join 없이 post_id 로 where 조건에 넣어 검색한다', async () => {
      // given
      const post = postFactory.makeOne();
      post.comments.add(...commentFactory.make(5));
      await postEntityRepository.persistAndFlush(post);
      orm.em.clear();

      // when
      const result = await commentEntityRepository
        .createQueryBuilder('comment')
        .select(['id', 'post'])
        .where({ post: { id: post.id } })
        .getSingleResult();

      // then
      expect(result?.post.id).toBe(post.id);
    });

    it('join 관계 테이블 컬럼 직접 지정해서 select', async () => {
      // given
      const post = postFactory.makeOne();
      post.comments.add(...commentFactory.make(5));
      await postEntityRepository.persistAndFlush(post);
      orm.em.clear();

      // when
      const result = await commentEntityRepository.findOneOrFail(
        { post: { id: { $lte: post.id } } },
        {
          populate: ['post'],
          // fields: ['id', 'like', 'post', { post: ['name'] }],
          strategy: LoadStrategy.JOINED,
          fields: ['id', 'like', 'post', 'post.name'],
        },
      );

      // then
      expect(result.post.isInitialized()).toBe(true);
      expect(result.post.unwrap().name).toBe(post.name);
    });

    it('queryBuilder 로 join 관계 테이블 컬럼 직접 지정해서 select', async () => {
      // given
      const post = postFactory.makeOne();
      post.comments.add(...commentFactory.make(5));
      await postEntityRepository.persistAndFlush(post);
      orm.em.clear();

      // when
      const result = await commentEntityRepository
        .createQueryBuilder('c')
        .select(['c.id', 'c.like', 'post', 'p.name'])
        .join('c.post', 'p')
        .getSingleResult();

      // then
      expect(result?.post.isInitialized()).toBe(false);
      expect(result?.post.unwrap().name).toBeUndefined();
    });
  });

  it('transaction 테스트', async () => {
    // given
    const post = postFactory.makeOne();
    post.comments.add(...commentFactory.make(5));
    await postEntityRepository.persistAndFlush(post);
    orm.em.clear();
    const newPost = await postEntityRepository.findOneOrFail(post.id, {
      populate: ['comments'],
    });

    // when
    await orm.em.transactional(async () => {
      newPost.content = 'new content';
      newPost.comments[0]!.like = 12345;
    });

    // then
    orm.em.clear();
    const postResult = await postEntityRepository.findOneOrFail(post.id);
    const commentResult = await commentEntityRepository.findOneOrFail(
      post.comments[0]?.id || 0n,
    );
    expect(postResult.content).toBe('new content');
    expect(commentResult.like).toBe(12345);
  });
});
