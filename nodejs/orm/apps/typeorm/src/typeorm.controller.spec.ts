import { Test, TestingModule } from '@nestjs/testing';
import { TypeormController } from './typeorm.controller';
import { TypeormService } from './typeorm.service';

describe('TypeormController', () => {
  let typeormController: TypeormController;

  beforeEach(async () => {
    const app: TestingModule = await Test.createTestingModule({
      controllers: [TypeormController],
      providers: [TypeormService],
    }).compile();

    typeormController = app.get<TypeormController>(TypeormController);
  });

  describe('root', () => {
    it('should return "Hello World!"', () => {
      expect(typeormController.getHello()).toBe('Hello World!');
    });
  });
});
