import { Test, TestingModule } from '@nestjs/testing';
import { MikroormController } from './mikroorm.controller';
import { MikroormService } from './mikroorm.service';

describe('MikroormController', () => {
  let mikroormController: MikroormController;

  beforeEach(async () => {
    const app: TestingModule = await Test.createTestingModule({
      controllers: [MikroormController],
      providers: [MikroormService],
    }).compile();

    mikroormController = app.get<MikroormController>(MikroormController);
  });

  describe('root', () => {
    it('should return "Hello World!"', () => {
      expect(mikroormController.getHello()).toBe('Hello World!');
    });
  });
});
