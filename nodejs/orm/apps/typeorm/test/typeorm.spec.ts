import { Test, TestingModule } from '@nestjs/testing';
import { getRepositoryToken } from '@nestjs/typeorm';
import { Post } from '../src/post/post.entity';
import { Comment } from '../src/comment/comment.entity';
import { TypeormModule } from '../src/typeorm.module';
import { getConnection, getManager, Repository } from 'typeorm';
import { PostFactory } from '../src/factory/post-factory';
import { CommentFactory } from '../src/factory/comment-factory';
import { LocalDateTime } from '@js-joda/core';
import { PostStatus } from '../src/post/post-status';

describe('TypeORM', () => {
  let postRepository: Repository<Post>;
  let commentRepository: Repository<Comment>;

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [TypeormModule],
    }).compile();

    postRepository = moduleFixture.get(getRepositoryToken(Post));
    commentRepository = moduleFixture.get(getRepositoryToken(Comment));
  });

  afterAll(() => getConnection().close());

  beforeEach(() =>
    Promise.all([postRepository.clear(), commentRepository.clear()]),
  );

  it('정상적으로 저장한다', async () => {
    // given
    const post = PostFactory.make();
    post.comments = [CommentFactory.make(), CommentFactory.make()];

    // when
    await postRepository.save(post);

    // then
    const result = await postRepository
      .createQueryBuilder('post')
      .innerJoinAndSelect('post.comments', 'comment')
      .where({ id: post.id })
      .getOneOrFail();
    expect(result.id).toBe(post.id);
    expect(result.comments).toHaveLength(2);
  });

  describe('LocalDateTime', () => {
    it('repository 사용하는 경우 정상적으로 동작한다 ', async () => {
      // given
      const createdAt = LocalDateTime.of(2022, 2, 1);
      const post = PostFactory.make({ createdAt });
      post.comments = [CommentFactory.make(), CommentFactory.make()];
      await postRepository.save({
        ...post,
        updatedAt: LocalDateTime.now(),
      });

      // when
      const result = await postRepository.findOneOrFail({
        where: { createdAt },
        relations: ['comments'],
      });

      // then
      expect(result.comments).toHaveLength(2);
    });

    it('queryBuilder where 에 json 형태로 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      const createdAt = LocalDateTime.of(2022, 2, 1);
      const post = PostFactory.make({ createdAt });
      post.comments = [CommentFactory.make(), CommentFactory.make()];
      await postRepository.save({
        ...post,
        updatedAt: LocalDateTime.now(),
      });

      // when
      const result = await postRepository
        .createQueryBuilder('post')
        .where({ createdAt })
        .getMany();

      // then
      expect(result).toHaveLength(1);
    });

    it('queryBuilder where 에 string 형태로 사용하는 경우 조회에 실패한다', async () => {
      // given
      const createdAt = LocalDateTime.of(2022, 2, 1);
      const post = PostFactory.make({ createdAt });
      await postRepository.save({
        ...post,
        updatedAt: LocalDateTime.now(),
      });

      // when
      const result = await postRepository
        .createQueryBuilder('post')
        .where('post.createdAt = :createdAt', { createdAt })
        .getMany();

      // then
      expect(result).toHaveLength(0);
    });
  });

  describe('EClass', () => {
    it('where 에 json 형태로 사용하는 경우 정상적으로 동작한다', async () => {
      // given
      const status = PostStatus.PRIVATE;
      const post = PostFactory.make({ status });
      await postRepository.save(post);

      // when
      const result = await postRepository
        .createQueryBuilder('post')
        .where({ status })
        .getOneOrFail();

      // then
      expect(result.status).toBe(post.status);
    });

    it('string 형태로 사용하는 경우 조회에 실패한다', async () => {
      // given
      const status = PostStatus.PUBLIC;
      const post = PostFactory.make({ status });
      await postRepository.save(post);

      // when
      const result = await postRepository
        .createQueryBuilder('post')
        .where('post.status = :status', { status })
        .getMany();

      // then
      expect(result).toHaveLength(0);
    });
  });

  describe('select', () => {
    it('inner join 없이 post_id 로 where 조건에 넣어 검색한다', async () => {
      // given
      const post = PostFactory.make();
      post.comments = [CommentFactory.make(), CommentFactory.make()];
      await postRepository.save(post);

      // when
      const result = await commentRepository
        .createQueryBuilder('comment')
        .select(['comment.id', 'comment.post.id'])
        .where({ post: { id: post.id } })
        .getOneOrFail();

      // then
      expect(result.post).toBeUndefined();
    });

    it('join 관계 테이블 컬럼 직접 지정해서 select', async () => {
      // given
      const post = PostFactory.make();
      post.comments = [CommentFactory.make(), CommentFactory.make()];
      await postRepository.save(post);

      // when
      const result = await commentRepository.findOneOrFail(
        { post: { id: post.id } },
        {
          relations: ['post'],
          select: ['id', 'like'],
          // select: ['id', 'like', 'post.id', 'post.name'], // type error
        },
      );

      // then
      expect(result.post).not.toBeUndefined();
    });

    it('queryBuilder 로 join 관계 테이블 컬럼 직접 지정해서 select', async () => {
      // given
      const post = PostFactory.make();
      post.comments = [CommentFactory.make(), CommentFactory.make()];
      await postRepository.save(post);

      // when
      const result = await commentRepository
        .createQueryBuilder('c')
        .select(['c.id', 'c.like', 'p.id', 'p.name'])
        .innerJoin('c.post', 'p')
        .getOneOrFail();

      // then
      expect(result?.post.name).toBe(post.name);
    });
  });

  it('transaction 테스트', async () => {
    // given
    const post = PostFactory.make();
    post.comments = [CommentFactory.make(), CommentFactory.make()];
    await postRepository.save(post);

    // when
    const update = () =>
      getManager().transaction(async (manager) => {
        post.content = 'new content';
        await manager.save(post);
      });

    // then
    await expect(update).rejects.toThrowError('violates not-null constraint');
  });

  it('save 에 json 으로 전달하면 BeforeInsert hook 이 동작하지 않는다', async () => {
    // given
    const post = PostFactory.make();
    const createdAt = LocalDateTime.of(2000, 1, 1);

    // when
    await postRepository.save({
      ...post,
      createdAt,
      updatedAt: LocalDateTime.now(),
    });

    // then
    const postResult = await postRepository.findOneOrFail();
    expect(postResult.createdAt.equals(createdAt)).toBe(true);
  });
});
