import { Factory, Faker } from '@mikro-orm/seeder';
import { Post } from '../post/post.entity';

export class PostFactory extends Factory<Post> {
  model = Post;

  definition(faker: Faker): Partial<Post> {
    return {
      name: faker.name.findName(),
      content: faker.lorem.sentence(5),
      memo: faker.lorem.sentence(3),
    };
  }
}
