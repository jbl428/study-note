import { Test, TestingModule } from '@nestjs/testing';
import { MikroormModule } from '../src/mikroorm.module';
import { INestApplication } from '@nestjs/common';
import { getRepositoryToken } from '@nestjs/typeorm';
import { EntityRepository } from '@mikro-orm/postgresql';
import { Post } from '../src/post/post.entity';
import { Comment } from '../src/comment/comment.entity';

describe('MikroORM', () => {
  let app: INestApplication;
  let postEntityRepository: EntityRepository<Post>;
  let commentEntityRepository: EntityRepository<Comment>;

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [MikroormModule],
    }).compile();

    postEntityRepository = moduleFixture.get(getRepositoryToken(Post));
    commentEntityRepository = moduleFixture.get(getRepositoryToken(Comment));
  });

  // beforeEach(() => Promise.all([postEntityRepository.]));

  it('should ', () => {});
});
