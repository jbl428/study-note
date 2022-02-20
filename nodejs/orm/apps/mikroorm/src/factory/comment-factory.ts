import { Factory, Faker } from '@mikro-orm/seeder';
import { Comment } from '../comment/comment.entity';

export class CommentFactory extends Factory<Comment> {
  model = Comment;

  definition(faker: Faker): Partial<Comment> {
    return {
      content: faker.lorem.sentence(5),
      like: faker.datatype.number(),
      memo: faker.lorem.sentence(3),
    };
  }
}
