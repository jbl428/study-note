import { Controller, Get } from '@nestjs/common';
import { TypeormService } from './typeorm.service';

@Controller()
export class TypeormController {
  constructor(private readonly typeormService: TypeormService) {}

  @Get()
  getHello(): string {
    return this.typeormService.getHello();
  }
}
