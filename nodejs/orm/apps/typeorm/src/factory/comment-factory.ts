import { Comment } from '../comment/comment.entity';
import { faker } from '@mikro-orm/seeder';

export class CommentFactory {
  static make(): Comment {
    return Object.assign(new Comment(), {
      content: faker.lorem.sentence(5),
      like: faker.datatype.number(),
      memo: faker.lorem.sentence(3),
    });
  }
}
