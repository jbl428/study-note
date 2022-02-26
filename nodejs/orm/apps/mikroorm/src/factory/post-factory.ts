import { Factory, Faker } from '@mikro-orm/seeder';
import { Post } from '../post/post.entity';
import { PostStatus } from '../post/post-status';

export class PostFactory extends Factory<Post> {
  model = Post;

  definition(faker: Faker): Partial<Post> {
    return {
      name: faker.name.findName(),
      content: faker.lorem.sentence(5),
      memo: faker.lorem.sentence(3),
      status: PostStatus.PUBLIC,
      extra: { foo: 'bar' },
      someIds: [],
    };
  }
}
